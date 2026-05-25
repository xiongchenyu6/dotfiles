#!/usr/bin/env bash
#
# Push the latest trainingdata-cli + README.md to the rustfs fae-tools bucket.
# After this runs:
#   curl https://s3.gz.autolife.ai:8444/fae-tools/trainingdata-cli
#   curl https://s3.gz.autolife.ai:8444/fae-tools/README.md
# serve the new versions (anonymous read allowed on these two keys only).
#
# Usage: ./update-fae-tools.sh [path-to-trainingdata-cli]
#   defaults to ~/Documents/github/autolife/TrainingDataUploader/trainingdata-cli
set -euo pipefail

HOST="${RUSTFS_SSH:-autolife@183.6.107.47}"
PORT="${RUSTFS_SSH_PORT:-2222}"
ALIAS="${RUSTFS_ALIAS:-rustfs}"
BUCKET="${FAE_TOOLS_BUCKET:-fae-tools}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
README_SRC="${SCRIPT_DIR}/files/fae-tools-README.md"
CLI_SRC="${1:-${HOME}/Documents/github/autolife/TrainingDataUploader/trainingdata-cli}"

[[ -f "$CLI_SRC" ]]    || { echo "ERROR: $CLI_SRC not found" >&2; exit 1; }
[[ -f "$README_SRC" ]] || { echo "ERROR: $README_SRC not found" >&2; exit 1; }

# Sanity check before pushing.
bash -n "$CLI_SRC" || { echo "ERROR: $CLI_SRC failed syntax check" >&2; exit 1; }

CLI_SHA=$(sha256sum "$CLI_SRC"    | awk '{print $1}')
RDM_SHA=$(sha256sum "$README_SRC" | awk '{print $1}')

echo "[INFO] uploading to ${ALIAS}/${BUCKET}:"
echo "       trainingdata-cli (sha256: ${CLI_SHA:0:12})"
echo "       README.md        (sha256: ${RDM_SHA:0:12})"

REMOTE_CLI="/tmp/trainingdata-cli.$$"
REMOTE_RDM="/tmp/fae-tools-README.md.$$"
scp -q -P "$PORT" "$CLI_SRC"    "${HOST}:${REMOTE_CLI}"
scp -q -P "$PORT" "$README_SRC" "${HOST}:${REMOTE_RDM}"

ssh -p "$PORT" "$HOST" "bash -s" <<EOF
set -euo pipefail
mc cp --quiet --attr "Content-Type=application/octet-stream" "${REMOTE_CLI}" "${ALIAS}/${BUCKET}/trainingdata-cli"
mc cp --quiet --attr "Content-Type=text/markdown; charset=utf-8" "${REMOTE_RDM}" "${ALIAS}/${BUCKET}/README.md"
rm -f "${REMOTE_CLI}" "${REMOTE_RDM}"

CLI_REMOTE=\$(mc cat "${ALIAS}/${BUCKET}/trainingdata-cli" | sha256sum | awk '{print \$1}')
RDM_REMOTE=\$(mc cat "${ALIAS}/${BUCKET}/README.md"        | sha256sum | awk '{print \$1}')
echo "[INFO] remote sha256: trainingdata-cli=\${CLI_REMOTE:0:12} README.md=\${RDM_REMOTE:0:12}"
if [[ "\$CLI_REMOTE" != "${CLI_SHA}" ]]; then
  echo "ERROR: trainingdata-cli sha mismatch after upload" >&2; exit 1
fi
if [[ "\$RDM_REMOTE" != "${RDM_SHA}" ]]; then
  echo "ERROR: README.md sha mismatch after upload" >&2; exit 1
fi
EOF

echo "[OK] fae-tools updated."
echo "     https://s3.gz.autolife.ai:8444/${BUCKET}/trainingdata-cli"
echo "     https://s3.gz.autolife.ai:8444/${BUCKET}/README.md"
