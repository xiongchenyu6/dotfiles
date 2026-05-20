#!/bin/bash
#
# Migrate restic snapshots from VersityGW into RustFS as plain folders.
#
# For each snapshot in the source restic repo, restores it to a scratch dir
# and uploads via mc to `s3://<robot-id>/<date>/...`. Tracks progress with
# per-snapshot marker files so it can be re-run safely after interruption.
#
# Runs entirely on loopback (127.0.0.1:7070 → 127.0.0.1:9000). No external
# traffic.
#
# Usage:
#   sudo /usr/local/bin/migrate-restic-to-rustfs.sh main           # training-backup → robot-* buckets (all snapshots)
#   sudo /usr/local/bin/migrate-restic-to-rustfs.sh main-latest    # only the latest snapshot per host
#   sudo /usr/local/bin/migrate-restic-to-rustfs.sh main-rest      # everything OTHER than the latest per host (historical fill-in)
#   sudo /usr/local/bin/migrate-restic-to-rustfs.sh audit          # training-audit-* → audit-archive
#   sudo /usr/local/bin/migrate-restic-to-rustfs.sh all            # main + audit (legacy default)
#   sudo /usr/local/bin/migrate-restic-to-rustfs.sh fast           # main-latest + audit  (≈ usable system, runs in ~1h)
#   sudo /usr/local/bin/migrate-restic-to-rustfs.sh full           # fast THEN main-rest (background fill-in)
#
# Designed to be invoked via:
#   systemd-run --no-block --unit=migrate-rustfs --uid=root \
#     /usr/local/bin/migrate-restic-to-rustfs.sh all
#
set -euo pipefail

# -----------------------------------------------------------------------------
# Config
# -----------------------------------------------------------------------------

MODE="${1:-all}"

# RUSTFS_ALIAS unused since switching from mc to s5cmd; kept for back-compat refs only.
RUSTFS_ALIAS="rustfs"

# VersityGW uses a plain-file POSIX backend, so each restic repo is a directory
# on disk. Read those directly with restic's `local:` mode — bypasses VGW HTTP
# entirely and gets us a 3-5x speedup on restore.
MAIN_REPO_PATH="/data/versitygw/training-backup"
AUDIT_234_REPO_PATH="/data/versitygw/training-audit-autolife-robot-234"
W9WLCU_REPO_PATH="/data/_archived/training-audit-20260304"

RESTIC_PASSWORD_VAL="x"

SCRATCH_ROOT="/data/scratch-restore"
LOG_DIR="/var/log/rustfs-migration"
LOG_FILE="${LOG_DIR}/migrate-$(date +%Y%m%d-%H%M%S).log"
S5CMD_PART_SIZE_MB="${S5CMD_PART_SIZE_MB:-256}"
S5CMD_CONCURRENCY="${S5CMD_CONCURRENCY:-5}"
S5CMD_NUMWORKERS="${S5CMD_NUMWORKERS:-32}"
RCLONE_CHUNK_SIZE="${RCLONE_CHUNK_SIZE:-64M}"

mkdir -p "$SCRATCH_ROOT" "$LOG_DIR"

# Credentials shared between rclone (marker ops) and s5cmd (bulk upload).
RUSTFS_SECRET=$(grep '^RUSTFS_SECRET_KEY=' /etc/rustfs/rustfs.env | cut -d= -f2-)
RUSTFS_ACCESS=$(grep '^RUSTFS_ACCESS_KEY=' /etc/rustfs/rustfs.env | cut -d= -f2-)
export AWS_ACCESS_KEY_ID="$RUSTFS_ACCESS"
export AWS_SECRET_ACCESS_KEY="$RUSTFS_SECRET"
export AWS_REGION="us-east-1"
S3_ENDPOINT_URL="http://127.0.0.1:9000"

# Rclone config for the small marker ops (cheap, no chunk concern).
RCLONE_CONFIG_FILE=$(mktemp)
chmod 600 "$RCLONE_CONFIG_FILE"
trap 'rm -f "$RCLONE_CONFIG_FILE"' EXIT
cat > "$RCLONE_CONFIG_FILE" <<EOF_RC
[s3]
type = s3
provider = Other
endpoint = ${S3_ENDPOINT_URL}
access_key_id = ${RUSTFS_ACCESS}
secret_access_key = ${RUSTFS_SECRET}
force_path_style = true
EOF_RC
export RCLONE_CONFIG="$RCLONE_CONFIG_FILE"

# -----------------------------------------------------------------------------
# Logging
# -----------------------------------------------------------------------------

# All output also goes to the log file. tee with -i to ignore SIGINT.
exec > >(tee -ia "$LOG_FILE") 2>&1

log()  { echo "[$(date '+%F %T')] $*"; }
warn() { echo "[$(date '+%F %T')] WARN $*" >&2; }
err()  { echo "[$(date '+%F %T')] ERROR $*" >&2; }

START_TS=$(date +%s)
trap 'rc=$?; log "exiting rc=$rc after $(($(date +%s)-START_TS))s — log: $LOG_FILE"' EXIT

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

# Returns the upload target as an rclone path "s3:<bucket>/<prefix>"
target_prefix_main() {
  local host="$1" date="$2"
  # robot bucket name: strip the "autolife-" prefix from hostname,
  # so autolife-robot-234 → robot-234
  local bucket="${host#autolife-}"
  echo "s3:${bucket}/${date}"
}

target_prefix_audit() {
  local host="$1" date="$2"
  local short="${host#autolife-}"
  echo "s3:audit-archive/${short}/${date}"
}

# Has this snapshot already been migrated?
# Marker object lives at <target>/_MIGRATED.json; rclone lsf returns 0 on found.
marker_exists() {
  local target="$1"
  rclone lsf --files-only "${target}/_MIGRATED.json" 2>/dev/null | grep -q '_MIGRATED'
}

mark_done() {
  local target="$1" snap_id="$2" host="$3" date="$4" bytes="$5"
  local marker
  marker=$(mktemp)
  cat > "$marker" <<EOF
{
  "snap_id": "${snap_id}",
  "hostname": "${host}",
  "snap_date": "${date}",
  "migrated_at": "$(date -u +%FT%TZ)",
  "src_bytes": ${bytes}
}
EOF
  rclone copyto "$marker" "${target}/_MIGRATED.json" --s3-chunk-size="$RCLONE_CHUNK_SIZE" 2>/dev/null
  rm -f "$marker"
}

# Wrapper for the bulk copy step.
# Targets like "s3:robot-234/2026-04-08" (rclone-style) — convert to "s3://robot-234/2026-04-08/".
rclone_to_s3uri() {
  # input: "s3:bucket/prefix"  →  "s3://bucket/prefix/"
  local r="$1"
  echo "s3://${r#s3:}/"
}

rclone_mirror() {
  local src="$1" rclone_dst="$2"
  local dst_uri
  dst_uri=$(rclone_to_s3uri "$rclone_dst")
  # s5cmd's `cp 'src/*'` recursively uploads (s5cmd-side glob, not shell glob).
  # --concurrency: parallel multipart parts per file
  # --numworkers: parallel files
  # --part-size 256 (MiB): side-steps the RustFS beta multipart bug seen with mc's 16 MiB parts
  s5cmd --endpoint-url="$S3_ENDPOINT_URL" \
        --numworkers "$S5CMD_NUMWORKERS" \
        cp --concurrency "$S5CMD_CONCURRENCY" \
           --part-size "$S5CMD_PART_SIZE_MB" \
           "${src}/*" "$dst_uri"
}

# Restore one restic snapshot to scratch_dir, then upload to target_prefix,
# then mark done. Cleans up scratch_dir at the end (always).
process_snapshot() {
  local repo="$1" snap_id="$2" host="$3" snap_date="$4" target="$5" snap_bytes="${6:-0}"

  if marker_exists "$target"; then
    log "SKIP (already migrated) snap=$snap_id host=$host date=$snap_date → $target"
    return 0
  fi

  local scratch="${SCRATCH_ROOT}/${snap_id}"
  log "RESTORE snap=$snap_id host=$host date=$snap_date scratch=$scratch"
  rm -rf "$scratch"
  mkdir -p "$scratch"

  local restic_rc=0
  RESTIC_REPOSITORY="$repo" \
  RESTIC_PASSWORD="$RESTIC_PASSWORD_VAL" \
    restic restore "$snap_id" --target "$scratch" --quiet 2>&1 || restic_rc=$?

  if [[ $restic_rc -ne 0 ]]; then
    err "restic restore failed snap=$snap_id rc=$restic_rc"
    rm -rf "$scratch"
    return 1
  fi

  # restic restores with the absolute path; e.g. snapshot path was
  # /home/ubuntu/data/capture/...
  # → scratch/home/ubuntu/data/capture/...
  local src_root
  if [[ -d "$scratch/home/ubuntu/data/capture" ]]; then
    src_root="$scratch/home/ubuntu/data/capture"
  else
    # fallback: take the deepest single-child path
    src_root="$scratch"
    while true; do
      local kids
      kids=$(find "$src_root" -mindepth 1 -maxdepth 1 -printf "%f\n" 2>/dev/null | head -2)
      [[ $(echo "$kids" | wc -l) -eq 1 && -d "$src_root/$kids" ]] || break
      src_root="$src_root/$kids"
    done
  fi

  local restored_bytes
  restored_bytes=$(du -sb "$src_root" 2>/dev/null | cut -f1)
  log "UPLOAD snap=$snap_id bytes=${restored_bytes} src=$src_root → $target"

  local up_rc=0
  rclone_mirror "$src_root" "$target" 2>&1 || up_rc=$?

  if [[ $up_rc -ne 0 ]]; then
    err "rclone upload failed snap=$snap_id rc=$up_rc"
    rm -rf "$scratch"
    return 1
  fi

  mark_done "$target" "$snap_id" "$host" "$snap_date" "${restored_bytes:-0}"
  rm -rf "$scratch"
  log "DONE snap=$snap_id host=$host date=$snap_date"
  return 0
}

# -----------------------------------------------------------------------------
# Mode handlers
# -----------------------------------------------------------------------------

migrate_main() {
  local select_mode="${1:-all}"   # all | latest | rest
  log "=== MIGRATE main: ${MAIN_REPO_PATH} → robot-* (select=${select_mode}) ==="

  local repo="local:${MAIN_REPO_PATH}"
  local snapshots_json
  snapshots_json=$(
    RESTIC_REPOSITORY="$repo" \
    RESTIC_PASSWORD="$RESTIC_PASSWORD_VAL" \
      restic snapshots --json
  )

  local total ok fail skipped
  total=$(echo "$snapshots_json" | jq length)
  ok=0; fail=0; skipped=0
  log "total snapshots in $MAIN_REPO_PATH: $total"

  # Build the iteration row set based on select_mode.
  # latest = max(time) per hostname; rest = everything else; all = all snaps.
  local rows
  case "$select_mode" in
    latest)
      rows=$(echo "$snapshots_json" | jq -r '
        group_by(.hostname)
        | map(max_by(.time))
        | sort_by(.time)[]
        | [.id, .hostname, (.time|split("T")[0])] | @tsv')
      ;;
    rest)
      rows=$(echo "$snapshots_json" | jq -r '
        group_by(.hostname)
        | map( (max_by(.time) | .id) as $latest_id | map(select(.id != $latest_id)) )
        | flatten | sort_by(.time)[]
        | [.id, .hostname, (.time|split("T")[0])] | @tsv')
      ;;
    *)  # all
      rows=$(echo "$snapshots_json" | jq -r '
        sort_by(.time)[]
        | [.id, .hostname, (.time|split("T")[0])] | @tsv')
      ;;
  esac
  local selected_count
  selected_count=$(printf '%s\n' "$rows" | sed '/^$/d' | wc -l)
  log "select_mode=$select_mode selected $selected_count of $total snapshots"

  while IFS=$'\t' read -r snap_id host snap_date; do
    [[ -z "$snap_id" ]] && continue
    local target
    target=$(target_prefix_main "$host" "$snap_date")
    if marker_exists "$target"; then
      skipped=$((skipped+1))
      log "SKIP $snap_id $host $snap_date (marker present)"
      continue
    fi
    if process_snapshot "$repo" "$snap_id" "$host" "$snap_date" "$target"; then
      ok=$((ok+1))
    else
      fail=$((fail+1))
    fi
    log "PROGRESS main: ok=$ok fail=$fail skipped=$skipped of $total"
  done <<<"$rows"

  log "=== MAIN DONE: ok=$ok fail=$fail skipped=$skipped of $total ==="
  return 0
}

migrate_audit_234() {
  log "=== MIGRATE audit-234: ${AUDIT_234_REPO_PATH} → audit-archive/robot-234 ==="

  local repo="local:${AUDIT_234_REPO_PATH}"
  local snapshots_json
  snapshots_json=$(
    RESTIC_REPOSITORY="$repo" \
    RESTIC_PASSWORD="$RESTIC_PASSWORD_VAL" \
      restic snapshots --json 2>&1
  )

  if ! echo "$snapshots_json" | jq -e 'type=="array"' >/dev/null 2>&1; then
    warn "audit-234 repo: no readable snapshots ($snapshots_json)"
    return 0
  fi

  local total=0 ok=0 fail=0 skipped=0
  total=$(echo "$snapshots_json" | jq length)
  log "total audit-234 snapshots: $total"

  local rows
  rows=$(echo "$snapshots_json" | jq -r '
    sort_by(.time)[]
    | [.id, .hostname, (.time|split("T")[0])] | @tsv')

  while IFS=$'\t' read -r snap_id host snap_date; do
    [[ -z "$snap_id" ]] && continue
    local target
    target=$(target_prefix_audit "$host" "$snap_date")
    if marker_exists "$target"; then
      skipped=$((skipped+1))
      continue
    fi
    # process_snapshot uses SRC_*_MAIN env vars; override repo with audit creds inline
    local scratch="${SCRATCH_ROOT}/${snap_id}"
    rm -rf "$scratch"; mkdir -p "$scratch"
    log "RESTORE (audit) snap=$snap_id host=$host date=$snap_date"
    local rc=0
    RESTIC_REPOSITORY="$repo" \
    RESTIC_PASSWORD="$RESTIC_PASSWORD_VAL" \
      restic restore "$snap_id" --target "$scratch" --quiet || rc=$?
    if [[ $rc -ne 0 ]]; then
      err "audit restic restore failed snap=$snap_id rc=$rc"
      fail=$((fail+1))
      rm -rf "$scratch"; continue
    fi
    local src_root="$scratch/home/ubuntu/data/capture"
    [[ -d "$src_root" ]] || src_root="$scratch"
    local restored_bytes
    restored_bytes=$(du -sb "$src_root" 2>/dev/null | cut -f1)
    log "UPLOAD (audit) snap=$snap_id bytes=${restored_bytes} → $target"
    if rclone_mirror "$src_root" "$target"; then
      mark_done "$target" "$snap_id" "$host" "$snap_date" "${restored_bytes:-0}"
      ok=$((ok+1))
      log "DONE (audit) snap=$snap_id"
    else
      err "audit rclone upload failed snap=$snap_id"
      fail=$((fail+1))
    fi
    rm -rf "$scratch"
  done <<<"$rows"

  log "=== AUDIT-234 DONE: ok=$ok fail=$fail skipped=$skipped of $total ==="
}

migrate_audit_w9wlcu() {
  log "=== MIGRATE audit-w9wlcu: ${W9WLCU_REPO_PATH} (archived local) → audit-archive/w9wlcu ==="

  if [[ ! -d "$W9WLCU_REPO_PATH" ]]; then
    warn "no archived w9wlcu dir at $W9WLCU_REPO_PATH — skipping"
    return 0
  fi

  # Try opening as a restic repo against the local filesystem path. The
  # password is the same "x" used elsewhere.
  local repo="local:${W9WLCU_REPO_PATH}"
  # The archived dir IS a restic repo (data/ index/ keys/ snapshots/ config).
  local snapshots_json
  snapshots_json=$(
    RESTIC_REPOSITORY="$repo" \
    RESTIC_PASSWORD="$RESTIC_PASSWORD_VAL" \
      restic snapshots --json 2>&1
  )

  if ! echo "$snapshots_json" | jq -e 'type=="array"' >/dev/null 2>&1; then
    warn "w9wlcu repo unreadable: $snapshots_json"
    return 0
  fi

  local total ok=0 fail=0 skipped=0
  total=$(echo "$snapshots_json" | jq length)
  log "total w9wlcu snapshots: $total"

  local rows
  rows=$(echo "$snapshots_json" | jq -r '
    sort_by(.time)[]
    | [.id, .hostname, (.time|split("T")[0])] | @tsv')

  while IFS=$'\t' read -r snap_id host snap_date; do
    [[ -z "$snap_id" ]] && continue
    # All w9wlcu data sits under audit-archive/w9wlcu/<date>/  (the host name
    # in the snapshot is the source host; we keep it as info in the marker only)
    local target="s3:audit-archive/w9wlcu/${snap_date}"
    if marker_exists "$target"; then
      skipped=$((skipped+1))
      continue
    fi
    local scratch="${SCRATCH_ROOT}/${snap_id}"
    rm -rf "$scratch"; mkdir -p "$scratch"
    log "RESTORE (w9wlcu) snap=$snap_id date=$snap_date host=$host"
    local rc=0
    RESTIC_REPOSITORY="$repo" \
    RESTIC_PASSWORD="$RESTIC_PASSWORD_VAL" \
      restic restore "$snap_id" --target "$scratch" --quiet || rc=$?
    if [[ $rc -ne 0 ]]; then
      err "w9wlcu restic restore failed snap=$snap_id rc=$rc"
      fail=$((fail+1)); rm -rf "$scratch"; continue
    fi
    local src_root="$scratch/home/ubuntu/data/capture"
    [[ -d "$src_root" ]] || src_root="$scratch"
    local restored_bytes
    restored_bytes=$(du -sb "$src_root" 2>/dev/null | cut -f1)
    log "UPLOAD (w9wlcu) bytes=${restored_bytes} → $target"
    if rclone_mirror "$src_root" "$target"; then
      mark_done "$target" "$snap_id" "$host" "$snap_date" "${restored_bytes:-0}"
      ok=$((ok+1))
    else
      err "w9wlcu rclone upload failed snap=$snap_id"
      fail=$((fail+1))
    fi
    rm -rf "$scratch"
  done <<<"$rows"

  log "=== W9WLCU DONE: ok=$ok fail=$fail skipped=$skipped of $total ==="
}

# -----------------------------------------------------------------------------
# Entry
# -----------------------------------------------------------------------------

log "Migration start, mode=$MODE"
log "Free space: $(df -h /data | tail -1)"

case "$MODE" in
  main)        migrate_main all ;;
  main-latest) migrate_main latest ;;
  main-rest)   migrate_main rest ;;
  audit)       migrate_audit_234; migrate_audit_w9wlcu ;;
  all)         migrate_main all; migrate_audit_234; migrate_audit_w9wlcu ;;
  fast)        migrate_main latest; migrate_audit_234; migrate_audit_w9wlcu ;;
  full)        migrate_main latest; migrate_audit_234; migrate_audit_w9wlcu; migrate_main rest ;;
  *)           err "unknown mode: $MODE"; exit 2 ;;
esac

log "All work done. See $LOG_FILE for full log."
