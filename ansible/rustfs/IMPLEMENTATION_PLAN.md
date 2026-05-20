# RustFS Migration — Implementation Plan

Goal: replace VersityGW + restic with RustFS + plain-file bucket-per-robot
layout. Drop restic encryption/dedup in favor of direct S3 folder access.

## Stage 1: Install RustFS alongside VersityGW

**Status**: Complete

**Goal**: RustFS running on `:9000` (S3) + `:9001` (console), VGW untouched on `:7070`.

**Deliverables**:
- `deploy-rustfs.yml` — Ansible playbook
- `secrets/rustfs.yaml` — sops-encrypted (root_access_key=`rustfsadmin`, 32-char random secret)
- systemd unit `rustfs.service`
- /data/rustfs/vol1 storage volume, system user `rustfs`
- UFW open for 9000/9001 (host-level; cloud SG still closed externally)

**Notes**:
- `RUSTFS_CONSOLE_ENABLE=true` (not `on`) — RustFS rejects `on`/`off`
- Console served at `/rustfs/console/` (Next.js app), not root path
- Cloud security group still blocks 9000/9001 externally — this is intentional per
  the migration plan (no external exposure tonight)

## Stage 2: Casdoor OIDC wiring

**Status**: Deferred to next session

**Goal**: Human users in `organization_9mukfn` log into RustFS console via Casdoor SSO.

**Blocker**: Console must be reachable from a browser to test OIDC redirect flow;
cloud SG is intentionally closed tonight. Will wire when external access is opened.

**Prep done**:
- Casdoor OIDC discovery confirmed at `https://43.139.62.96/.well-known/openid-configuration`
- Env-var pattern documented:
  ```
  RUSTFS_IDENTITY_OPENID_ENABLE=on
  RUSTFS_IDENTITY_OPENID_CONFIG_URL=https://43.139.62.96/.well-known/openid-configuration
  RUSTFS_IDENTITY_OPENID_CLIENT_ID=…    # from Casdoor app
  RUSTFS_IDENTITY_OPENID_CLIENT_SECRET= # sops
  RUSTFS_IDENTITY_OPENID_SCOPES=openid,profile,email
  RUSTFS_IDENTITY_OPENID_GROUPS_CLAIM=groups
  ```
- sops file `secrets/rustfs.yaml` already has placeholder slots for client_id and client_secret

## Stage 3: Static accounts + buckets

**Status**: Complete

**Deliverable**: `setup-rustfs-iam.yml` — idempotent Ansible playbook.

**Bucket layout** (15 buckets):
- `robot-001`, `robot-234`, `robot-239`, `robot-249`, `robot-250`, `robot-252`,
  `robot-255`, `robot-256`, `robot-260`, `robot-263`, `robot-267`, `robot-268`,
  `robot-269`, `robot-270` (one per migrated hostname)
- `audit-archive` (consolidated audit data, prefix-per-subject inside)

**Users + policies**:
| User | Policy | Scope |
|------|--------|-------|
| `myaccess` | `consoleAdmin` | full admin |
| `fae-uploader` | `fae-writer` | r/w on `robot-*`, `audit-archive`, `lerobot-*` |
| `fae-uploader-v2` | `fae-writer` | same (used by Phase 2 lerobot script) |
| `audit-reviewer-autolife-robot-234` | `audit-robot-234` | read `robot-234`, `audit-archive/robot-234/*` |
| `audit-reviewer-w9wlcu` | `audit-w9wlcu` | read `audit-archive/w9wlcu/*` (NEW secret: `audit-secret-w9wlcu-20260514`) |

Phase 1 access keys are reused verbatim so the `TrainingDataUploader` scripts only need endpoint swaps, not credential rotation.

## Stage 4: Data migration

**Status**: In progress (running as transient systemd unit `rustfs-migration.service` in `full` mode)

**Deliverable**: `migrate-restic-to-rustfs.sh` — orchestrator with per-snapshot resume markers.

**Modes**:
- `main-latest` — only the most recent snapshot per host (14 snapshots, ~12 hours)
- `main-rest` — all OTHER snapshots (95 snapshots, days)
- `main` — all snapshots in the main repo
- `audit` — both audit repos
- `fast` — main-latest + audit (≈1-day usable system)
- `full` — fast THEN main-rest (currently in use)

**Approach** per snapshot:
1. `restic restore` (local: mode, plain-file repo path) to `/data/scratch-restore/<snap-id>/`
2. `mc mirror` to `rustfs/robot-<id>/<YYYY-MM-DD>/`
3. Drop `_MIGRATED.json` marker
4. Cleanup scratch dir

**Key speedup**: restic uses `local:/data/versitygw/training-backup` (plain POSIX backend) instead of S3 HTTP loopback.

**Scope**: 109 main snapshots + 1 audit-234 + N audit-w9wlcu.
~865 GB deduped source → expected ~5 TB restored on RustFS.
Disk has 118 TiB free.

**Throughput observed**: restic restore ~60 MB/s, mc upload to RustFS ~20 MB/s.
At sequential pace, all 109 snapshots = days. Hence the `full` mode does latest-first
to ensure a usable system by morning.

**Resume**: re-run `migrate-restic-to-rustfs.sh full` — completed snapshots have markers and are skipped.

## Stage 5: Script cutover + decommission

**Status**: Scripts rewritten; decommission deferred

**Script changes** (in `~/Documents/github/autolife/TrainingDataUploader/`):
- `legacy-restic/` — old restic-based scripts moved here for 2-week rollback window
- `fetch-training-data.sh` — rewritten (rclone, folder-layout, no restic). New CLI: `hosts | list | download`
- `fetch-audit-data.sh` — rewritten similarly
- `share-training-data.sh` — rewritten as server-side `mc mirror` between `robot-*` and `audit-archive`
- `deploy-direct-upload.sh` — NEW robot-side uploader replacing `deploy-restic.sh`. Uploads to `s3://robot-<id>/<YYYY-MM-DD>/` daily via rclone in an 8 PM–6 AM window.
- `deploy-lerobot-upload.sh` — endpoint swapped 7070 → 9000 (Phase 2 was already rclone-based)
- `admin-create-lerobot-bucket.sh` — rewritten to use `mc` over SSH (was using `versitygw admin`)
- `README.md`, `AUDIT-GUIDE.md` — fully rewritten for the new flow

**Decommission**: Per user's revised choice (after I flagged RustFS beta status),
keep `/data/versitygw/` + binary for 2 weeks after migration verifies, then delete.
Tonight: leave VGW running on :7070 for rollback.

**Robot redeployment**: `sudo ./deploy-direct-upload.sh install` on each of the 14
robots when ready (replaces the restic flow).
