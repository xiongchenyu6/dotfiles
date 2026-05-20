#!/bin/bash
#
# Convenience: check the running rustfs-migration unit's progress.
# Just runs over SSH; no state of its own.
#
set -euo pipefail

HOST="${RUSTFS_SSH:-autolife@183.6.107.47}"
PORT="${RUSTFS_SSH_PORT:-2222}"

ssh -p "$PORT" "$HOST" 'sudo bash -c "
echo --- service ---
systemctl is-active rustfs-migration 2>&1
systemctl show rustfs-migration --property=ActiveState,SubState,ExecMainStartTimestamp,ExecMainStatus 2>&1
echo
echo --- per-snapshot counters ---
LATEST=\$(ls -t /var/log/rustfs-migration/migrate-*.log 2>/dev/null | head -1)
echo log: \$LATEST
echo \"  RESTORE started: \$(grep -c \\\"RESTORE snap=\\\" \$LATEST)\"
echo \"  DONE:            \$(grep -c \\\"DONE snap=\\\" \$LATEST)\"
echo \"  SKIP (resumed):  \$(grep -c \\\"SKIP\\\" \$LATEST)\"
echo \"  ERRORS:          \$(grep -cE \\\"^.*ERROR\\\" \$LATEST)\"
echo
echo --- bucket sizes ---
mc du rustfs/ 2>&1 | head -30
echo
echo --- last 12 log lines ---
tail -n 12 \$LATEST
echo
echo --- scratch + uptime ---
du -sh /data/scratch-restore /data/rustfs/vol1 2>/dev/null
uptime
"'
