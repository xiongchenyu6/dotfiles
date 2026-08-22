#!/usr/bin/env bash
set -euo pipefail

# Deploys the Cloudflare Worker that publishes the apex's /.well-known/matrix
# documents, so Matrix identifiers read @user:starslab.qzz.io while the
# homeserver itself runs on matrix.starslab.qzz.io.
#
# The apex is a Cloudflare Pages site this repository does not build, so the
# delegation is served by a Worker on a route scoped to that path prefix
# rather than by a file in the site. Everything else on the apex is untouched.
#
# Authentication uses the wrangler OAuth login already on this machine
# (`wrangler login` refreshes it when the access token has expired). The
# DNS-01 token in secrets/common.yaml cannot do this — it only carries
# Zone -> DNS -> Edit.

ZONE_NAME="${ZONE_NAME:-starslab.qzz.io}"
WORKER_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)/ansible/tuwunel/well-known-worker"

usage() {
	echo "Usage: $0 [--dry-run]"
	echo "Deploys the Matrix well-known Worker to the apex of $ZONE_NAME."
	echo
	echo "Environment overrides: ZONE_NAME, CLOUDFLARE_ACCOUNT_ID"
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
	usage
	exit 0
fi

DRY_RUN=false
[[ "${1:-}" == "--dry-run" ]] && DRY_RUN=true

for bin in wrangler jq python3; do
	command -v "$bin" >/dev/null || {
		echo "missing dependency: $bin" >&2
		exit 1
	}
done

# This Cloudflare login owns several accounts, so resolve the one holding the
# zone rather than making wrangler guess.
if [[ -z "${CLOUDFLARE_ACCOUNT_ID:-}" ]]; then
	oauth_token="$(
		python3 - <<'PY'
import os, tomllib
path = os.path.expanduser("~/.config/.wrangler/config/default.toml")
try:
    print(tomllib.load(open(path, "rb")).get("oauth_token", ""))
except OSError:
    print("")
PY
	)"
	[[ -n "$oauth_token" ]] || {
		echo "no wrangler login found; run 'wrangler login' first" >&2
		exit 1
	}
	CLOUDFLARE_ACCOUNT_ID="$(curl -sS -H "Authorization: Bearer $oauth_token" \
		"https://api.cloudflare.com/client/v4/zones?name=$ZONE_NAME" |
		jq -r '.result[0].account.id // empty')"
fi

[[ -n "$CLOUDFLARE_ACCOUNT_ID" ]] || {
	echo "could not resolve the account owning $ZONE_NAME." >&2
	echo "The wrangler access token may have expired; run 'wrangler login'." >&2
	exit 1
}
export CLOUDFLARE_ACCOUNT_ID
echo "==> account $CLOUDFLARE_ACCOUNT_ID owns $ZONE_NAME"

if $DRY_RUN; then
	echo "would deploy $WORKER_DIR to the route $ZONE_NAME/.well-known/matrix/*"
	exit 0
fi

(cd "$WORKER_DIR" && wrangler deploy)

echo
echo "==> verifying"
for doc in server client; do
	url="https://$ZONE_NAME/.well-known/matrix/$doc"
	printf '%-52s %s\n' "$url" "$(curl -sL --max-time 15 "$url")"
done
