"""Offline regression fixtures; never read a real user's conversation in tests."""

import importlib.util
import json
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

SCRIPT = (
    Path(__file__).resolve().parents[2]
    / "stow-managed/ai-skills/.claude/skills"
    / "resume-codex/scripts/read_session.py"
)
spec = importlib.util.spec_from_file_location("handoff", SCRIPT)
handoff = importlib.util.module_from_spec(spec)
spec.loader.exec_module(handoff)


class HandoffTests(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        self.base = Path(self.tmp.name)
        self.project = self.base / "project"
        self.project.mkdir()
        self.cache = self.base / "cache"

    def write(self, source, session, entries, cwd=None, child=False, subdir=""):
        cwd = str(cwd or self.project)
        if source == "codex":
            path = self.cache / "sessions/2026/10/04" / f"rollout-{session}.jsonl"
            meta = {
                "type": "session_meta",
                "payload": {
                    "id": session,
                    "cwd": cwd,
                    "timestamp": "2026-10-01T00:00:00Z",
                    "source": {"subagent": {}} if child else "cli",
                },
            }
        else:
            path = self.cache / "projects/encoded-project" / subdir / f"{session}.jsonl"
            meta = {
                "type": "system",
                "sessionId": session,
                "cwd": cwd,
                "timestamp": "2026-10-01T00:00:00Z",
                "isSidechain": child,
            }
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("".join(json.dumps(e) + "\n" for e in [meta, *entries]))
        return path

    def message(self, source, role, text, **extra):
        if source == "codex":
            return {
                "timestamp": "2026-10-04T00:00:00Z",
                "type": "response_item",
                "payload": {
                    "type": "message",
                    "role": role,
                    "content": [{"type": "input_text", "text": text}],
                    **extra,
                },
            }
        return {
            "timestamp": "2026-10-04T00:00:00Z",
            "type": role,
            "message": {"role": role, "content": text},
            **extra,
        }

    def read(self, source, path, **options):
        return handoff.extract(handoff.metadata(path, source), source, **options)

    def test_latest_activity_not_creation_or_touch_time(self):
        older = self.write("codex", "old", [])
        newer = self.write(
            "codex", "active", [self.message("codex", "user", "Continue this task")]
        )
        os.utime(older, (1999999999, 1999999999))
        self.write("codex", "child", [], child=True)
        other = self.base / "other"
        other.mkdir()
        self.write("codex", "other", [], cwd=other)
        selected = handoff.discover("codex", self.cache, self.project)
        self.assertEqual([s["id"] for s in selected], ["active", "old"])
        self.assertEqual(selected[0]["path"], str(newer))

    def test_claude_excludes_subagents_and_sidechains(self):
        self.write("claude", "main", [])
        self.write("claude", "side", [], child=True)
        self.write("claude", "agent", [], subdir="subagents")
        self.assertEqual(
            [s["id"] for s in handoff.discover("claude", self.cache, self.project)],
            ["main"],
        )

    def test_no_automatic_cross_project_fallback(self):
        other = self.base / "other"
        other.mkdir()
        self.write("codex", "other", [], cwd=other)
        self.assertEqual(handoff.discover("codex", self.cache, self.project), [])
        self.assertEqual(
            len(handoff.discover("codex", self.cache, self.project, session="other")), 1
        )

    def test_nested_repositories_stay_separate(self):
        subprocess.run(["git", "init", "-q", str(self.project)], check=True)
        nested = self.project / "nested"
        subprocess.run(["git", "init", "-q", str(nested)], check=True)
        subdir = self.project / "src"
        subdir.mkdir()
        self.write("codex", "nested", [], cwd=nested)
        self.write("codex", "same", [], cwd=subdir)
        self.assertEqual(
            [s["id"] for s in handoff.discover("codex", self.cache, self.project)],
            ["same"],
        )

    def test_codex_filters_reasoning_and_system_instructions(self):
        path = self.write(
            "codex",
            "main",
            [
                self.message("codex", "developer", "INJECTED_RULE"),
                self.message(
                    "codex", "user", "# AGENTS.md instructions\nINJECTED_RULE"
                ),
                self.message("codex", "user", "Fix the failing parser"),
                self.message(
                    "codex", "assistant", "PRIVATE_REASONING", channel="analysis"
                ),
                self.message(
                    "codex", "assistant", "Parser patch ready", channel="final"
                ),
                {
                    "type": "response_item",
                    "payload": {
                        "type": "function_call",
                        "name": "shell",
                        "arguments": "DO_NOT_EXECUTE",
                    },
                },
            ],
        )
        report = self.read("codex", path)
        rendered = json.dumps(report)
        for excluded in ("INJECTED_RULE", "PRIVATE_REASONING", "DO_NOT_EXECUTE"):
            self.assertNotIn(excluded, rendered)
        self.assertEqual(
            report["latest_user_request"]["text"], "Fix the failing parser"
        )
        self.assertIn(
            "DO_NOT_EXECUTE", json.dumps(self.read("codex", path, include_tools=True))
        )

    def test_claude_summary_tool_results_and_thinking(self):
        path = self.write(
            "claude",
            "main",
            [
                self.message("claude", "user", "Original task"),
                self.message(
                    "claude", "user", "Summary: tests pending", isCompactSummary=True
                ),
                self.message(
                    "claude",
                    "assistant",
                    [
                        {"type": "thinking", "thinking": "PRIVATE_REASONING"},
                        {"type": "text", "text": "Implemented parser"},
                        {
                            "type": "tool_use",
                            "name": "Bash",
                            "input": {"command": "test parser"},
                        },
                    ],
                ),
                self.message(
                    "claude",
                    "user",
                    [{"type": "tool_result", "content": "TEST_FAILURE"}],
                ),
                self.message("claude", "user", "Now fix the failing test"),
            ],
        )
        report = self.read("claude", path)
        self.assertEqual(report["latest_summary"]["text"], "Summary: tests pending")
        self.assertEqual(
            report["latest_user_request"]["text"], "Now fix the failing test"
        )
        self.assertNotIn("PRIVATE_REASONING", json.dumps(report))
        self.assertNotIn("TEST_FAILURE", json.dumps(report))
        self.assertIn(
            "TEST_FAILURE", json.dumps(self.read("claude", path, include_tools=True))
        )

    def test_truncated_record_and_read_only(self):
        path = self.write(
            "codex", "main", [self.message("codex", "user", "Keep working")]
        )
        with path.open("a") as stream:
            stream.write('{"unfinished":')
        before = path.read_bytes()
        report = self.read("codex", path)
        self.assertEqual(report["latest_user_request"]["text"], "Keep working")
        self.assertIn("Malformed", " ".join(report["warnings"]))
        self.assertEqual(before, path.read_bytes())

    def test_opaque_compaction_and_plaintext_summary(self):
        path = self.write(
            "codex",
            "main",
            [
                {
                    "type": "compacted",
                    "payload": {
                        "message": "Tests pending",
                        "replacement_history": [
                            {"type": "compaction", "encrypted_content": "OPAQUE_SECRET"}
                        ],
                    },
                },
                self.message("codex", "user", "Continue tests"),
            ],
        )
        report = self.read("codex", path)
        self.assertEqual(report["latest_summary"]["text"], "Tests pending")
        self.assertIn("Encrypted", " ".join(report["warnings"]))
        self.assertNotIn("OPAQUE_SECRET", json.dumps(report))

    def test_event_only_latest_user_survives_mixed_log(self):
        path = self.write(
            "codex",
            "main",
            [
                self.message("codex", "assistant", "Earlier answer"),
                {
                    "type": "event_msg",
                    "payload": {"type": "user_message", "message": "New correction"},
                },
            ],
        )
        self.assertEqual(
            self.read("codex", path)["latest_user_request"]["text"], "New correction"
        )

    def test_oversized_record_is_skipped(self):
        path = self.write(
            "claude",
            "main",
            [
                self.message("claude", "assistant", "x" * (handoff.MAX_LINE + 1)),
                self.message("claude", "user", "Latest request survives"),
            ],
        )
        report = self.read("claude", path)
        self.assertIn("Oversized", " ".join(report["warnings"]))
        self.assertEqual(
            report["latest_user_request"]["text"], "Latest request survives"
        )

    def test_budget_preserves_request_and_summary_anchors(self):
        path = self.write(
            "codex",
            "main",
            [
                self.message("codex", "user", "Implement handoff"),
                {"type": "compacted", "payload": {"message": "Keep local changes"}},
                *[
                    self.message("codex", "assistant", "Progress " + "x" * 500)
                    for _ in range(100)
                ],
                self.message("codex", "user", "Only change the parser"),
            ],
        )
        report = self.read("codex", path, max_chars=4000)
        self.assertLessEqual(
            len(json.dumps(report, ensure_ascii=False, indent=2)), 4000
        )
        self.assertTrue(report["truncated"])
        self.assertEqual(report["first_user_request"]["text"], "Implement handoff")
        self.assertEqual(report["latest_summary"]["text"], "Keep local changes")
        self.assertEqual(
            report["latest_user_request"]["text"], "Only change the parser"
        )

    def test_secret_redaction(self):
        value = (
            "password: fixture-password\n密码是fixture-chinese-password\nAPI_KEY=fixture-token\n"
            "Bearer fixture-bearer\nhttps://user:fixture-pass@example.com\n"
            "sk-abcdefghijklmnop\n-----BEGIN PRIVATE KEY-----\nfixture-key\n-----END PRIVATE KEY-----"
        )
        redacted = handoff.redact(value)
        for secret in (
            "fixture-password",
            "fixture-chinese-password",
            "fixture-token",
            "fixture-bearer",
            "fixture-pass",
            "fixture-key",
            "sk-abcdefghijklmnop",
        ):
            self.assertNotIn(secret, redacted)

    def test_cli_environment_home_and_metadata_only_list(self):
        self.write(
            "claude", "main", [self.message("claude", "user", "DO_NOT_PRINT_IN_LIST")]
        )
        env = dict(os.environ, CLAUDE_CONFIG_DIR=str(self.cache))
        result = subprocess.run(
            [
                sys.executable,
                str(SCRIPT),
                "--source",
                "claude",
                "--cwd",
                str(self.project),
                "--list",
            ],
            env=env,
            capture_output=True,
            text=True,
            check=True,
        )
        self.assertEqual(json.loads(result.stdout)[0]["id"], "main")
        self.assertNotIn("DO_NOT_PRINT_IN_LIST", result.stdout)
        rejected = subprocess.run(
            [sys.executable, str(SCRIPT), "--source", "claude", "--all-projects"],
            env=env,
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(rejected.returncode, 2)


if __name__ == "__main__":
    unittest.main()
