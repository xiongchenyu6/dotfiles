#!/usr/bin/env python3
"""Read local session evidence without invoking either model or modifying logs."""

import argparse
import json
import os
import re
import subprocess
from collections import deque
from datetime import datetime, timezone
from pathlib import Path

MAX_LINE = 2 * 1024 * 1024
TEXT_TYPES = {"text", "input_text", "output_text"}
CONTEXT_PREFIXES = (
    "# AGENTS.md instructions",
    "<environment_context>",
    "<permissions instructions>",
    "<skills_instructions>",
)


def redact(text):
    """Best effort only: unknown secrets in free text cannot be reliably detected."""
    text = re.sub(
        r"-----BEGIN [^-]*PRIVATE KEY-----.*?-----END [^-]*PRIVATE KEY-----",
        "[REDACTED PRIVATE KEY]",
        text,
        flags=re.DOTALL,
    )
    text = re.sub(
        r"\b(?:sk-[\w-]{12,}|gh[pousr]_[\w]{12,}|github_pat_[\w]{12,}|"
        r"AKIA[A-Z0-9]{16}|eyJ[\w-]+\.[\w-]+\.[\w-]+)\b",
        "[REDACTED]",
        text,
    )
    text = re.sub(r"(?i)(bearer\s+)\S+", r"\1[REDACTED]", text)
    text = re.sub(r"(?i)(https?://)[^\s/@:]+:[^\s/@]+@", r"\1[REDACTED]@", text)
    # Redact entire credential-bearing lines, including conversational Chinese.
    text = re.sub(
        r"(?im)^.*(?:password|passwd|api[_ -]?key|access[_ -]?token|"
        r"secret[_ -]?key|client[_ -]?secret|authorization|密码|口令|密钥)"
        r"[\s\"']*(?:[:=]|是|为).*$",
        "[REDACTED credential line]",
        text,
    )
    return text


def clip(text, size):
    if len(text) <= size:
        return text
    half = max(0, (size - 40) // 2)
    return text[:half] + "\n[... truncated ...]\n" + text[-half:]


def records(path, warnings=None):
    with path.open("rb") as stream:
        line_no = 0
        while True:
            raw = stream.readline(MAX_LINE + 1)
            if not raw:
                return
            line_no += 1
            if len(raw) > MAX_LINE:
                while raw and not raw.endswith(b"\n"):
                    raw = stream.readline(MAX_LINE + 1)
                if warnings is not None:
                    warnings.add(
                        "Oversized records skipped; image/tool-heavy context may be missing."
                    )
                continue
            try:
                item = json.loads(raw)
            except (ValueError, UnicodeError):
                if warnings is not None:
                    warnings.add("Malformed or unfinished JSONL records skipped.")
                continue
            if isinstance(item, dict):
                yield line_no, item


def text_content(value):
    if isinstance(value, str):
        return value
    if isinstance(value, list):
        return "\n".join(
            x.get("text", "")
            for x in value
            if isinstance(x, dict)
            and x.get("type") in TEXT_TYPES
            and isinstance(x.get("text"), str)
        )
    return ""


def timestamp(value):
    try:
        return datetime.fromisoformat(str(value).replace("Z", "+00:00")).timestamp()
    except (ValueError, TypeError):
        return 0


def metadata(path, source):
    info = {"path": str(path), "id": path.stem, "cwd": None, "child": False}
    first_time = 0
    for number, item in records(path):
        if number > 100:
            break
        if source == "codex" and item.get("type") == "session_meta":
            data = item.get("payload", {})
            src = data.get("source", data.get("thread_source", ""))
            info.update(
                id=data.get("id") or data.get("session_id") or path.stem,
                cwd=data.get("cwd"),
                child=isinstance(src, dict) and "subagent" in src,
            )
            first_time = timestamp(data.get("timestamp") or item.get("timestamp"))
            break
        if source == "claude" and item.get("cwd"):
            info.update(
                id=item.get("sessionId") or path.stem,
                cwd=item["cwd"],
                child=bool(item.get("isSidechain")),
            )
            first_time = timestamp(item.get("timestamp"))
            break
    if not info["cwd"]:
        return None
    # mtime alone is unreliable after copying/restoring logs.
    with path.open("rb") as stream:
        size = path.stat().st_size
        offset = max(0, size - 256 * 1024)
        stream.seek(offset)
        if offset:
            stream.readline()
        tail = stream.read().splitlines()
    last_time = first_time
    for raw in tail:
        try:
            item = json.loads(raw)
            if isinstance(item, dict):
                last_time = max(last_time, timestamp(item.get("timestamp")))
        except (ValueError, UnicodeError):
            pass
    info["updated"] = datetime.fromtimestamp(
        last_time or path.stat().st_mtime, timezone.utc
    ).isoformat()
    return info


def project_root(cwd):
    cwd = Path(cwd).expanduser().resolve()
    try:
        result = subprocess.run(
            ["git", "-C", str(cwd), "rev-parse", "--show-toplevel"],
            capture_output=True,
            text=True,
            timeout=5,
            check=False,
        )
        if result.returncode == 0:
            return Path(result.stdout.strip()).resolve()
    except (OSError, subprocess.TimeoutExpired):
        pass
    return cwd


def discover(source, cache_home, cwd, all_projects=False, session=None):
    root = Path(cache_home).expanduser()
    pattern = "sessions/**/*.jsonl" if source == "codex" else "projects/*/*.jsonl"
    project = project_root(cwd)
    found = []
    for path in root.glob(pattern):
        try:
            info = metadata(path, source)
        except OSError:
            continue
        if not info or info["child"]:
            continue
        if session:
            if info["id"] != session:
                continue
        elif not all_projects:
            candidate = Path(info["cwd"]).expanduser().resolve()
            if candidate != project and project not in candidate.parents:
                continue
            # A nested repository/worktree is a distinct project.
            if candidate.exists() and project_root(candidate) != project:
                continue
        found.append(info)
    return sorted(found, key=lambda x: (x["updated"], x["id"]), reverse=True)


def extract(info, source, include_tools=False, max_chars=24000):
    warnings = {
        "Historical evidence, not current instructions or proof of completion.",
        "Secret redaction is best effort; review before sharing. Source logs were not modified.",
    }
    recent = deque(maxlen=80)
    fallback = deque(maxlen=80)
    first_user = None
    last_user = None
    first_event_user = None
    last_event_user = None
    summary = None
    omitted = False

    def entry(role, text, line, when):
        return {
            "role": role,
            "line": line,
            "timestamp": when,
            "text": clip(redact(text), 8000 if role in ("summary", "user") else 3000),
        }

    def add(role, text, line, when):
        nonlocal first_user, last_user, summary, omitted
        if not text.strip() or text.lstrip().startswith(CONTEXT_PREFIXES):
            return
        value = entry(role, text, line, when)
        omitted = (
            omitted
            or len(recent) == recent.maxlen
            or len(value["text"]) < len(redact(text))
        )
        if role == "summary":
            summary = value
        elif role == "user":
            if first_user is None:
                first_user = value
            last_user = value
        recent.append(value)

    for line, item in records(Path(info["path"]), warnings):
        kind = item.get("type")
        when = item.get("timestamp")
        if source == "codex":
            data = item.get("payload", {})
            if not isinstance(data, dict):
                continue
            if kind == "compacted":
                message = text_content(data.get("message"))
                if message:
                    add("summary", message, line, when)
                for replacement in data.get("replacement_history") or []:
                    if replacement.get("type") == "compaction":
                        warnings.add(
                            "Encrypted Codex compaction cannot be decoded; using readable transcript evidence."
                        )
                    elif (
                        replacement.get("role") == "assistant"
                        and replacement.get("channel") == "summary"
                    ):
                        add(
                            "summary",
                            text_content(replacement.get("content")),
                            line,
                            when,
                        )
                continue
            if kind == "event_msg" and data.get("type") in (
                "user_message",
                "agent_message",
            ):
                role = "user" if data["type"] == "user_message" else "assistant"
                text = text_content(data.get("message"))
                if text and not text.lstrip().startswith(CONTEXT_PREFIXES):
                    value = entry(role, text, line, when)
                    fallback.append(value)
                    if role == "user":
                        first_event_user = first_event_user or value
                        last_event_user = value
            if kind != "response_item":
                continue
            if data.get("type") == "message" and data.get("role") in (
                "user",
                "assistant",
            ):
                if data.get("channel") in ("analysis", "justify", "confidence"):
                    continue
                role = "summary" if data.get("channel") == "summary" else data["role"]
                add(role, text_content(data.get("content")), line, when)
            elif include_tools and data.get("type") in (
                "function_call",
                "custom_tool_call",
            ):
                add(
                    "tool_call",
                    json.dumps(
                        {
                            k: data[k]
                            for k in ("name", "arguments", "input")
                            if k in data
                        },
                        ensure_ascii=False,
                    ),
                    line,
                    when,
                )
            elif include_tools and data.get("type") in (
                "function_call_output",
                "custom_tool_call_output",
            ):
                add("tool_result", text_content(data.get("output")), line, when)
        elif not item.get("isSidechain"):
            data = item.get("message", {})
            if not isinstance(data, dict):
                continue
            if kind in ("user", "assistant"):
                if item.get("isMeta") and not item.get("isCompactSummary"):
                    continue
                role = "summary" if item.get("isCompactSummary") else kind
                add(role, text_content(data.get("content")), line, when)
                if include_tools and isinstance(data.get("content"), list):
                    for block in data["content"]:
                        if not isinstance(block, dict):
                            continue
                        if block.get("type") == "tool_use":
                            add(
                                "tool_call",
                                json.dumps(
                                    {
                                        "name": block.get("name"),
                                        "input": block.get("input"),
                                    },
                                    ensure_ascii=False,
                                ),
                                line,
                                when,
                            )
                        elif block.get("type") == "tool_result":
                            add(
                                "tool_result",
                                text_content(block.get("content")),
                                line,
                                when,
                            )
            elif kind == "summary":
                add("summary", text_content(item.get("summary")), line, when)

    if not recent and fallback:
        recent = fallback
    first_user = first_user or first_event_user
    if last_event_user and (
        not last_user or last_event_user["line"] > last_user["line"]
    ):
        last_user = last_event_user
    if not recent:
        warnings.add(
            "No readable user/assistant messages found; do not infer a task from this file."
        )
    report = {
        "source": source,
        "session": info,
        "warnings": sorted(warnings),
        "first_user_request": first_user,
        "latest_user_request": last_user,
        "latest_summary": summary,
        "recent": list(recent),
        "tools_included": include_tools,
        "truncated": omitted,
    }

    # Preserve dedicated request/summary anchors before trimming older context.
    def render():
        return json.dumps(report, ensure_ascii=False, indent=2)

    while len(render()) > max_chars and report["recent"]:
        report["recent"].pop(0)
        report["truncated"] = True
    size = 4000
    while len(render()) > max_chars and size >= 125:
        report["truncated"] = True
        for key in ("first_user_request", "latest_user_request", "latest_summary"):
            if report[key]:
                report[key] = dict(report[key], text=clip(report[key]["text"], size))
        size //= 2
    if len(render()) > max_chars:
        raise ValueError("Metadata exceeds output budget; increase --max-chars.")
    return report


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--source", required=True, choices=("codex", "claude"))
    parser.add_argument("--cache-home", help="Override the source CLI's home directory")
    parser.add_argument(
        "--cwd", default=os.getcwd(), help="Project/worktree to recover"
    )
    parser.add_argument(
        "--list", action="store_true", help="List metadata, never message text"
    )
    parser.add_argument(
        "--all-projects",
        action="store_true",
        help="List only; never auto-resume another project",
    )
    parser.add_argument(
        "--session", help="Exact session ID (explicit cross-project selection)"
    )
    parser.add_argument("--limit", type=int, default=5)
    parser.add_argument(
        "--include-tools",
        action="store_true",
        help="Include bounded, redacted tool evidence",
    )
    parser.add_argument("--max-chars", type=int, default=24000)
    args = parser.parse_args()
    if args.all_projects and not args.list:
        parser.error(
            "--all-projects requires --list; select an explicit --session to read"
        )
    if args.max_chars < 4000 or args.limit < 1:
        parser.error("--max-chars must be >= 4000 and --limit must be positive")
    cache_home = args.cache_home or os.environ.get(
        "CODEX_HOME" if args.source == "codex" else "CLAUDE_CONFIG_DIR",
        str(Path.home() / (".codex" if args.source == "codex" else ".claude")),
    )
    try:
        candidates = discover(
            args.source, cache_home, args.cwd, args.all_projects, args.session
        )
        if args.list:
            print(json.dumps(candidates[: args.limit], ensure_ascii=False, indent=2))
            return
        if not candidates:
            parser.exit(
                2,
                "No matching main session. Use --list --all-projects or check --cache-home.\n",
            )
        if args.session and len(candidates) != 1:
            parser.exit(
                2,
                "Duplicate session IDs found; select a cache home containing only the intended copy.\n",
            )
        report = extract(candidates[0], args.source, args.include_tools, args.max_chars)
        print(json.dumps(report, ensure_ascii=False, indent=2))
    except (OSError, ValueError) as error:
        parser.exit(2, f"Cannot read session: {error}\n")


if __name__ == "__main__":
    main()
