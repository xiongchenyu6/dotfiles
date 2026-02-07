#!/usr/bin/env bash
set -euo pipefail

# Bootstrap a new NixOS host folder, update flake + sops, and optionally deploy.

usage() {
  cat <<'EOF'
Usage:
  bootstrap-nixos-host.sh --host <name> --target <user@ip> [options]

Required:
  --host <name>            Host name (e.g. huoshan-bj-002)
  --target <user@ip>       Target SSH host (e.g. root@115.190.173.250)

Options:
  --build-host <user@ip>   Build host for nixos-rebuild (defaults to --target)
  --flake <flake-ref>      Flake for nixos-anywhere (default: .#generic-nixos)
  --template <dir>         Template dir to copy (default: nixos-configurations/generic-nixos)
  --skip-generate          Skip nixos-anywhere --generate-hardware-config
  --skip-sops              Skip .sops.yaml update and sops updatekeys
  --no-sops                Alias for --skip-sops
  --rebuild                Run nixos-rebuild switch after setup
  --force                  Overwrite existing host dir or temp hardware file
  -h, --help               Show this help

Examples:
  bootstrap-nixos-host.sh --host huoshan-bj-002 --target root@1.2.3.4 --rebuild
EOF
}

HOST=""
TARGET=""
BUILD=""
FLAKE=".#generic-nixos"
TEMPLATE_DIR="nixos-configurations/generic-nixos"
SKIP_GENERATE="false"
SKIP_SOPS="false"
RUN_REBUILD="false"
FORCE="false"

while [[ $# -gt 0 ]]; do
  case "$1" in
    --host) HOST="${2:-}"; shift 2 ;;
    --target) TARGET="${2:-}"; shift 2 ;;
    --build-host) BUILD="${2:-}"; shift 2 ;;
    --flake) FLAKE="${2:-}"; shift 2 ;;
    --template) TEMPLATE_DIR="${2:-}"; shift 2 ;;
    --skip-generate) SKIP_GENERATE="true"; shift ;;
    --skip-sops|--no-sops) SKIP_SOPS="true"; shift ;;
    --rebuild) RUN_REBUILD="true"; shift ;;
    --force) FORCE="true"; shift ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown arg: $1" >&2; usage; exit 1 ;;
  esac
done

if [[ -z "$HOST" || -z "$TARGET" ]]; then
  echo "Missing --host or --target" >&2
  usage
  exit 1
fi

HOST_DIR="nixos-configurations/$HOST"
HARDWARE_TMP="./hardware-configuration.nix"

if [[ -e "$HOST_DIR" && "$FORCE" != "true" ]]; then
  echo "Host dir exists: $HOST_DIR (use --force to overwrite)" >&2
  exit 1
fi

if [[ -e "$HARDWARE_TMP" && "$FORCE" != "true" ]]; then
  echo "Temp hardware file exists: $HARDWARE_TMP (use --force to overwrite)" >&2
  exit 1
fi

if [[ "$SKIP_GENERATE" != "true" ]]; then
  nixos-anywhere \
    --generate-hardware-config nixos-generate-config "$HARDWARE_TMP" \
    --flake "$FLAKE" \
    "$TARGET"
fi

mkdir -p "$HOST_DIR"

if [[ -d "$TEMPLATE_DIR" ]]; then
  for f in "$TEMPLATE_DIR"/*.nix; do
    [[ -f "$f" ]] || continue
    cp -f "$f" "$HOST_DIR/$(basename "$f")"
  done
else
  echo "Template dir not found: $TEMPLATE_DIR" >&2
  exit 1
fi

if [[ -f "$HARDWARE_TMP" ]]; then
  mv -f "$HARDWARE_TMP" "$HOST_DIR/hardware-configuration.nix"
fi

python - "$HOST" <<'PY'
import sys

host = sys.argv[1]
path = "flake.nix"

with open(path, "r", encoding="utf-8") as f:
  lines = f.read().splitlines()

needle = f"{host} = hostConfig;"
if any(needle in line for line in lines):
  sys.exit(0)

start = None
for i, line in enumerate(lines):
  if "nixos.hosts" in line:
    start = i
    break

if start is None:
  print("Could not find nixos.hosts in flake.nix", file=sys.stderr)
  sys.exit(1)

brace = None
for i in range(start, len(lines)):
  if lines[i].strip() == "{":
    brace = i
    break

if brace is None:
  print("Could not find nixos.hosts block start", file=sys.stderr)
  sys.exit(1)

end = None
for i in range(brace + 1, len(lines)):
  if lines[i].strip() == "};":
    end = i
    break

if end is None:
  print("Could not find nixos.hosts block end", file=sys.stderr)
  sys.exit(1)

indent = None
for i in range(brace + 1, end):
  if "hostConfig;" in lines[i]:
    indent = lines[i][:len(lines[i]) - len(lines[i].lstrip())]
    break

if indent is None:
  indent = "            "

lines.insert(end, f"{indent}{host} = hostConfig;")

with open(path, "w", encoding="utf-8") as f:
  f.write("\n".join(lines) + "\n")
PY

AGE_KEY=""
if [[ "$SKIP_SOPS" != "true" ]]; then
  if command -v ssh-keyscan >/dev/null 2>&1 && command -v ssh-to-age >/dev/null 2>&1; then
    TARGET_HOST="${TARGET#*@}"
    AGE_KEY="$(ssh-keyscan -t ed25519,rsa "$TARGET_HOST" 2>/dev/null | ssh-to-age 2>/dev/null || true)"
  else
    echo "ssh-keyscan or ssh-to-age not found; skipping .sops.yaml update" >&2
  fi
fi

if [[ -n "$AGE_KEY" && "$SKIP_SOPS" != "true" ]]; then
  python - "$HOST" "$AGE_KEY" <<'PY'
import sys

host = sys.argv[1]
age_key = sys.argv[2]
path = ".sops.yaml"

with open(path, "r", encoding="utf-8") as f:
  lines = f.read().splitlines()

anchor = f"&{host}"
ref = f"*{host}"

# Update keys list
if not any(anchor in line for line in lines):
  try:
    keys_idx = lines.index("keys:")
  except ValueError:
    print("No keys: section in .sops.yaml", file=sys.stderr)
    sys.exit(1)
  insert_at = keys_idx + 1
  while insert_at < len(lines):
    line = lines[insert_at]
    if line.startswith("  - "):
      insert_at += 1
      continue
    break
  lines.insert(insert_at, f"  - {anchor} {age_key}")

# Update each creation_rules age list
i = 0
while i < len(lines):
  line = lines[i]
  if line.strip() == "- age:":
    j = i + 1
    found = False
    while j < len(lines):
      s = lines[j].strip()
      if s.startswith("- *"):
        if ref in s:
          found = True
        j += 1
        continue
      if s.startswith("pgp:"):
        break
      if s.startswith("- ") and not s.startswith("- *"):
        break
      j += 1
    if not found:
      lines.insert(j, f"          - {ref}")
      i = j
  i += 1

with open(path, "w", encoding="utf-8") as f:
  f.write("\n".join(lines) + "\n")
PY

  if command -v sops >/dev/null 2>&1; then
    shopt -s nullglob
    for f in secrets/*.yaml secrets/*.env; do
      sops updatekeys "$f"
    done
  else
    echo "sops not found; skipping updatekeys" >&2
  fi
elif [[ "$SKIP_SOPS" != "true" ]]; then
  echo "Could not derive age key via ssh-keyscan | ssh-to-age; skipping .sops.yaml update" >&2
fi

if [[ "$RUN_REBUILD" == "true" ]]; then
  BUILD_ARGS=()
  if [[ -n "$BUILD" ]]; then
    BUILD_ARGS+=(--build-host "$BUILD")
  fi
  nixos-rebuild switch \
    --flake ".#${HOST}" \
    --target-host "$TARGET" \
    "${BUILD_ARGS[@]}" \
    --use-substitutes \
    --impure
fi

echo "Done. Host directory: $HOST_DIR"
