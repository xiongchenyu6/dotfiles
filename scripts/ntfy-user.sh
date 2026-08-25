#!/usr/bin/env bash
set -euo pipefail

# Manages the ntfy roster for lubancat. The roster lives in the repository's
# sops store under `ntfy.users`, not in ntfy's auth database, so a rebuilt host
# comes back with the same people on it and the whole thing is reviewable.
#
# This repository is public, which is why the roster is encrypted rather than
# sitting in ansible/ntfy/vars/main.yml.
#
# Passwords are generated here, hashed on the server with `ntfy user hash`, and
# printed once. Only the bcrypt hash is stored.

REMOTE="${REMOTE:-lubancat}"

usage() {
	cat <<'USAGE'
Usage:
  ntfy-user.sh list
  ntfy-user.sh add <username> [--admin] [--access <topic>:<permission>]...
  ntfy-user.sh passwd <username>
  ntfy-user.sh remove <username>

Permissions: rw | ro | wo | deny   (topic may use a * wildcard, e.g. 'alerts-*')

After any change, apply it:
  (cd ansible/ntfy && ansible-playbook -i inventory.ini deploy-ntfy.yml)
USAGE
}

[[ $# -ge 1 ]] || {
	usage
	exit 1
}

for bin in sops jq python3; do
	command -v "$bin" >/dev/null || {
		echo "missing dependency: $bin" >&2
		exit 1
	}
done

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
SECRETS="$REPO_ROOT/secrets/common.yaml"

roster_json() {
	sops -d --output-type json "$SECRETS" 2>/dev/null | jq -c '.ntfy.users // []'
}

write_roster() {
	# The whole `ntfy` map is replaced rather than the list inside it: sops set
	# rejects a bare list as a value. --value-stdin keeps the password hashes
	# out of the process listing.
	jq -c '{users: .}' <<<"$1" | sops set --value-stdin "$SECRETS" '["ntfy"]'
}

# ntfy refuses a hash it did not produce, so hash on the server itself.
hash_password() {
	# shellcheck disable=SC2016  # the $2a$ in the pattern is bcrypt's, not a variable
	printf '%s\n%s\n' "$1" "$1" |
		ssh "$REMOTE" 'docker exec -i ntfy ntfy user hash' |
		grep -oE '\$2[aby]\$[0-9]{2}\$[A-Za-z0-9./]{53}'
}

cmd="$1"
shift

case "$cmd" in
list)
	roster_json | jq -r '
      if length == 0 then "roster is empty"
      else .[] | "\(.name)\t\(.role // "user")\t\((.access // []) | map("\(.topic):\(.permission)") | join(" ") )"
      end' | column -t -s$'\t'
	;;

add)
	[[ $# -ge 1 ]] || {
		usage
		exit 1
	}
	name="$1"
	shift
	role="user"
	access='[]'
	while [[ $# -gt 0 ]]; do
		case "$1" in
		--admin)
			role="admin"
			shift
			;;
		--access)
			topic="${2%%:*}"
			perm="${2##*:}"
			access="$(jq -c --arg t "$topic" --arg p "$perm" '. + [{topic: $t, permission: $p}]' <<<"$access")"
			shift 2
			;;
		*)
			echo "unknown argument: $1" >&2
			exit 1
			;;
		esac
	done

	current="$(roster_json)"
	if jq -e --arg n "$name" 'any(.name == $n)' <<<"$current" >/dev/null; then
		echo "user $name already exists; use 'passwd' to reset the password" >&2
		exit 1
	fi

	password="$(python3 -c 'import secrets,string; a=string.ascii_letters+string.digits; print("".join(secrets.choice(a) for _ in range(20)))')"
	echo "==> hashing on $REMOTE"
	hash="$(hash_password "$password")"
	[[ -n "$hash" ]] || {
		echo "could not produce a password hash" >&2
		exit 1
	}

	updated="$(jq -c --arg n "$name" --arg h "$hash" --arg r "$role" --argjson a "$access" \
		'. + [{name: $n, role: $r, password_hash: $h, access: $a}]' <<<"$current")"
	write_roster "$updated"

	echo
	echo "  user:     $name ($role)"
	echo "  password: $password"
	echo
	echo "Shown once, and only the bcrypt hash was stored. Hand it over, then apply:"
	echo "  (cd $REPO_ROOT/ansible/ntfy && ansible-playbook -i inventory.ini deploy-ntfy.yml)"
	;;

passwd)
	[[ $# -eq 1 ]] || {
		usage
		exit 1
	}
	name="$1"
	current="$(roster_json)"
	jq -e --arg n "$name" 'any(.name == $n)' <<<"$current" >/dev/null || {
		echo "no such user: $name" >&2
		exit 1
	}
	password="$(python3 -c 'import secrets,string; a=string.ascii_letters+string.digits; print("".join(secrets.choice(a) for _ in range(20)))')"
	hash="$(hash_password "$password")"
	[[ -n "$hash" ]] || {
		echo "could not produce a password hash" >&2
		exit 1
	}
	write_roster "$(jq -c --arg n "$name" --arg h "$hash" \
		'map(if .name == $n then .password_hash = $h else . end)' <<<"$current")"
	echo
	echo "  user:     $name"
	echo "  password: $password"
	echo
	echo "Re-run the playbook to apply."
	;;

remove)
	[[ $# -eq 1 ]] || {
		usage
		exit 1
	}
	name="$1"
	current="$(roster_json)"
	jq -e --arg n "$name" 'any(.name == $n)' <<<"$current" >/dev/null || {
		echo "no such user: $name" >&2
		exit 1
	}
	write_roster "$(jq -c --arg n "$name" 'map(select(.name != $n))' <<<"$current")"
	echo "removed $name; re-run the playbook to apply"
	;;

*)
	usage
	exit 1
	;;
esac
