#!/usr/bin/env bash
set -euo pipefail

# Registers the Auth0 applications backing the lubancat self-hosted stack and
# writes the resulting credentials back into secrets/common.yaml (sops).
#
# Idempotent: an application that already exists is updated in place, so the
# client_id already stored in sops keeps working.
#
# The Auth0 machine-to-machine credentials come from secrets/common.yaml under
# auth0.management and need these Management API scopes:
#   create:clients read:clients update:clients

usage() {
	echo "Usage: $0 [--dry-run]"
	echo "Creates/updates the Tuwunel, Huly and AFFiNE applications in Auth0"
	echo "and stores their credentials in secrets/common.yaml."
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
	usage
	exit 0
fi

DRY_RUN=false
[[ "${1:-}" == "--dry-run" ]] && DRY_RUN=true

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SECRETS="$REPO_ROOT/secrets/common.yaml"

for bin in sops jq curl; do
	command -v "$bin" >/dev/null || {
		echo "missing dependency: $bin" >&2
		exit 1
	}
done

sops_get() { sops -d --extract "$1" "$SECRETS" 2>/dev/null || true; }

AUTH0_DOMAIN="$(sops_get '["auth0"]["domain"]')"
M2M_ID="$(sops_get '["auth0"]["management"]["client_id"]')"
M2M_SECRET="$(sops_get '["auth0"]["management"]["client_secret"]')"

if [[ -z "$AUTH0_DOMAIN" || -z "$M2M_ID" || -z "$M2M_SECRET" ]]; then
	echo "auth0.domain / auth0.management.* missing from $SECRETS" >&2
	exit 1
fi

echo "==> Requesting a Management API token from $AUTH0_DOMAIN"
TOKEN="$(curl -sS --request POST --url "https://$AUTH0_DOMAIN/oauth/token" \
	--header 'content-type: application/json' \
	--data "$(jq -n --arg id "$M2M_ID" --arg secret "$M2M_SECRET" --arg aud "https://$AUTH0_DOMAIN/api/v2/" \
		--arg scope "create:clients read:clients update:clients" \
		'{client_id: $id, client_secret: $secret, audience: $aud, grant_type: "client_credentials", scope: $scope}')" |
	jq -r '.access_token // empty')"

[[ -n "$TOKEN" ]] || {
	echo "could not obtain a Management API token" >&2
	exit 1
}

api() {
	local method="$1" path="$2"
	shift 2
	curl -sS -X "$method" "https://$AUTH0_DOMAIN/api/v2$path" \
		-H "Authorization: Bearer $TOKEN" \
		-H 'content-type: application/json' "$@"
}

# A dry run only prints what it would do, so it does not need the permission.
if ! $DRY_RUN && ! api GET '/clients?per_page=1' | jq -e 'type == "array"' >/dev/null; then
	echo "the management application lacks read:clients (and probably create:clients)." >&2
	echo "Grant create:clients, read:clients and update:clients to it in the Auth0" >&2
	echo "dashboard: Applications -> APIs -> Auth0 Management API -> Machine to Machine." >&2
	exit 1
fi

# sops key | display name | callback URL
# Tuwunel's callback embeds its own client_id, so it is patched after creation.
APPS=(
	"tuwunel|Tuwunel (Matrix)|https://matrix.starslab.qzz.io/_matrix/client/unstable/login/sso/callback/@CLIENT_ID@"
	"huly|Huly|https://huly.starslab.qzz.io/_accounts/auth/openid/callback"
	"affine|AFFiNE|https://affine.starslab.qzz.io/oauth/callback"
)

for entry in "${APPS[@]}"; do
	IFS='|' read -r key display callback <<<"$entry"
	origin="https://$(echo "$callback" | cut -d/ -f3)"

	echo "==> $display"

	if $DRY_RUN; then
		echo "    would create or update: $display"
		echo "    callback: $callback"
		continue
	fi

	existing="$(api GET "/clients?per_page=100&fields=client_id,name&include_fields=true" |
		jq -r --arg name "$display" '.[] | select(.name == $name) | .client_id' | head -1)"

	if [[ -z "$existing" ]]; then
		created="$(api POST '/clients' --data "$(jq -n --arg name "$display" \
			'{name: $name, app_type: "regular_web", oidc_conformant: true,
              grant_types: ["authorization_code", "refresh_token"],
              token_endpoint_auth_method: "client_secret_post",
              jwt_configuration: {alg: "RS256"}}')")"
		existing="$(echo "$created" | jq -r '.client_id // empty')"
		[[ -n "$existing" ]] || {
			echo "    failed: $(echo "$created" | jq -c .)" >&2
			exit 1
		}
		echo "    created client_id=$existing"
	else
		echo "    reusing client_id=$existing"
	fi

	resolved_callback="${callback//@CLIENT_ID@/$existing}"
	# jwt_configuration.alg is re-asserted on every run, not just at creation.
	# An Auth0 application that leaves it unset signs id_tokens with HS256,
	# which every client in this stack rejects — and the rejection surfaces as
	# a silent bounce back to the login page rather than as an error.
	api PATCH "/clients/$existing" --data "$(jq -n \
		--arg cb "$resolved_callback" --arg origin "$origin" \
		'{callbacks: [$cb], allowed_logout_urls: [$origin], web_origins: [$origin],
          jwt_configuration: {alg: "RS256"},
          allowed_origins: [$origin]}')" >/dev/null
	echo "    callback: $resolved_callback"

	secret="$(api GET "/clients/$existing?fields=client_secret&include_fields=true" | jq -r '.client_secret')"
	sops set "$SECRETS" "[\"auth0\"][\"$key\"]" \
		"$(jq -nc --arg id "$existing" --arg secret "$secret" '{client_id: $id, client_secret: $secret}')"
	echo "    stored in secrets/common.yaml under auth0.$key"
done

echo
echo "Done. Re-run the playbooks so the services pick the credentials up:"
echo "  for s in tuwunel huly affine; do"
echo "    (cd $REPO_ROOT/ansible/\$s && ansible-playbook -i inventory.ini deploy-\$s.yml)"
echo "  done"
