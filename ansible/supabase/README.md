# Supabase Stack — Ansible Deployment

Self-hosted **postgres + postgrest + gotrue + realtime** behind nginx.
Mirrors the NixOS module at `nixos-configurations/oracle-arm-002/postgres.nix`,
adapted for a Docker host (lubancat / Ubuntu 24.04 ARM64).

## Layout

| File                                | Purpose                                                              |
|-------------------------------------|----------------------------------------------------------------------|
| `deploy-supabase.yml`               | Main playbook                                                        |
| `inventory.ini`                     | Target host (`203.116.95.146`)                                       |
| `vars/main.yml`                     | Domains, image tags, ports, casdoor refs                             |
| `../../secrets/supabase.yaml`       | Casdoor `clientId` / `clientSecret` (sops-encrypted)                 |
| `templates/docker-compose.yml.j2`   | postgres + postgrest + auth + realtime                               |
| `templates/init-db.sql.j2`          | Roles, schemas, extensions (rerun-safe)                              |
| `templates/pgsodium_getkey.sh.j2`   | 64-char hex key generator for pgsodium (persists under PGDATA)       |
| `templates/nginx-supabase.conf.j2`  | TLS + reverse-proxy for the three vhosts                             |

## Pre-requisites

1. **DNS** (Cloudflare). All three records point at the host:
   - `api.starslab.qzz.io`
   - `auth.starslab.qzz.io`
   - `realtime.starslab.qzz.io`
2. **TLS cert** at `/etc/nginx/ssl/supabase.{crt,key}` covering all three names.
   Issued via acme.sh DNS-01 (CF token already saved in `/root/.acme.sh/account.conf`):
   ```bash
   /root/.acme.sh/acme.sh --issue --dns dns_cf \
     -d api.starslab.qzz.io \
     -d auth.starslab.qzz.io \
     -d realtime.starslab.qzz.io \
     --keylength ec-256
   /root/.acme.sh/acme.sh --install-cert -d api.starslab.qzz.io --ecc \
     --key-file /etc/nginx/ssl/supabase.key \
     --fullchain-file /etc/nginx/ssl/supabase.crt \
     --reloadcmd "systemctl reload nginx"
   ```
3. **Casdoor `supabase` application** with redirect URI
   `https://auth.starslab.qzz.io/callback`. Recreate via the Casdoor API:
   ```bash
   curl -c /tmp/cj -X POST https://casdoor.starslab.qzz.io/api/login \
     -H 'Content-Type: application/json' \
     -d '{"organization":"built-in","application":"app-built-in","username":"admin","password":"<pw>","autoSignin":true,"type":"login"}'
   curl -b /tmp/cj -X POST https://casdoor.starslab.qzz.io/api/add-application \
     -H 'Content-Type: application/json' \
     -d '{ "owner":"admin","name":"supabase","organization":"built-in","displayName":"Supabase",
           "clientId":"<random-hex-20>","clientSecret":"<random-hex-48>",
           "redirectUris":["https://auth.starslab.qzz.io/callback"],
           "grantTypes":["authorization_code","refresh_token"],
           "tokenFormat":"JWT","expireInHours":24,"refreshExpireInHours":168,
           "enablePassword":true,"enableSignUp":true,"enableSigninSession":true }'
   ```

## Usage

```bash
cd ansible/supabase
ansible-playbook -i inventory.ini deploy-supabase.yml
```

Override secrets at run time:
```bash
ansible-playbook -i inventory.ini deploy-supabase.yml \
  --extra-vars 'supabase_jwt_secret=...'
```

Secrets resolution order:
1. `--extra-vars` on the current run
2. Previously persisted `/opt/supabase/.secrets.yml` on the host
3. Freshly generated via `openssl rand`

## Public endpoints

| URL                                 | Service     | Notes                              |
|-------------------------------------|-------------|------------------------------------|
| `https://api.starslab.qzz.io`       | PostgREST   | OpenAPI at `/`, public views under `/public_*` are rate-limited |
| `https://auth.starslab.qzz.io`      | GoTrue      | `/signup`, `/token`, `/callback` … |
| `https://realtime.starslab.qzz.io`  | Realtime    | WebSocket at `/socket/websocket`   |

## Postgres extensions

Available out of the box (supabase/postgres image):
`pgsodium, pgjwt, pg_graphql, pgmq, pg_cron, pg_net, pg_tle, pg_stat_statements,
plpgsql_check, vector (pgvector), postgis, plv8, pg_repack, pg_hashids, hypopg,
index_advisor, pg_jsonschema, supabase_vault, wrappers, timescaledb`, plus
the standard contribs (`uuid-ossp`, `pgcrypto`, `hstore`, `citext`, `ltree`,
`btree_gin`, `btree_gist`, `pgtap`, `http`, …).

Not bundled (versus the NixOS module): `pg_hint_plan`, `pg_safeupdate`,
`plan_filter`, `wal2json`. Install via `pg_tle` if you need them.

## Casdoor SSO wiring (SAML 2.0)

GoTrue's SAML 2.0 SSO is enabled and Casdoor is registered as the IdP.

**How the playbook sets this up:**

1. The Casdoor `supabase` application's `redirectUris` includes
   `https://auth.starslab.qzz.io/sso/saml/acs` (the GoTrue Assertion
   Consumer Service) so Casdoor accepts SAML responses targeted there.
2. GoTrue runs with `GOTRUE_SAML_ENABLED=true` and a persistent PKCS#1 DER
   RSA key (`supabase_saml_private_key` in `.secrets.yml`) used to sign the
   AuthnRequest sent to Casdoor.
3. After the auth container is healthy, the playbook mints a service_role
   JWT (HS256-signed with `supabase_jwt_secret`) and `POST`s the Casdoor IdP
   to GoTrue's `/admin/sso/providers` — idempotent, skipped on subsequent
   runs once the metadata URL is already registered.

**Endpoints:**

| URL                                                          | Role                                |
|--------------------------------------------------------------|-------------------------------------|
| `https://auth.starslab.qzz.io/sso/saml/metadata`             | SP metadata (consumed by IdP)       |
| `https://auth.starslab.qzz.io/sso/saml/acs`                  | Assertion Consumer Service (POST)   |
| `https://auth.starslab.qzz.io/sso`                           | SP-initiated login (POST)           |
| `https://casdoor.starslab.qzz.io/api/saml/metadata?application=admin/supabase` | IdP metadata |
| `https://casdoor.starslab.qzz.io/login/saml/authorize/admin/supabase` | IdP SSO endpoint           |

**Initiating login** (browser-side, from your app):

```javascript
// supabase-js v2.x
const { data, error } = await supabase.auth.signInWithSSO({
  domain: 'casdoor.starslab.qzz.io',
})
window.location.href = data.url   // 303 to Casdoor SAML authorize
```

Or test from a shell:
```bash
curl -i -X POST https://auth.starslab.qzz.io/sso \
  -H 'Content-Type: application/json' \
  -d '{"domain":"casdoor.starslab.qzz.io"}'
# → HTTP 303 Location: https://casdoor.starslab.qzz.io/login/saml/authorize/admin/supabase?SAMLRequest=...&Signature=...
```

GoTrue runs email/password auth in parallel, so non-SSO users can still
sign up at `https://auth.starslab.qzz.io/signup`.

## Operations

- **Persisted secrets:** `/opt/supabase/.secrets.yml` (mode 0600). Survives
  re-runs; delete to rotate.
- **Backups:** `/etc/cron.d/supabase-pg-backup` — daily `pg_dump -Fc` →
  `/opt/supabase/backups/`, 14-day retention.
- **Restart a service:** `cd /opt/supabase && docker compose restart auth`.
- **Tail logs:** `docker logs -f supabase-{postgres,auth,postgrest,realtime}`.
- **Postgres CLI:** `docker exec -it supabase-postgres psql -U supabase_admin -d api`
  (local trust auth — no password).
- **Renew TLS cert:** acme.sh's daily cron auto-renews 30 days before expiry.

## Caveats discovered during deployment

- supabase/postgres needs `listen_addresses=*` overridden — its default is
  `localhost`, which breaks docker-network DNS lookups from the dependents.
- pgsodium requires a getkey script at
  `/usr/share/postgresql/extension/pgsodium_getkey` outputting a 64-char hex
  key. The bind-mounted script writes the key under `${PGDATA}` so it
  persists across container restarts.
- The image's init scripts strip `SUPERUSER` from the `postgres` role; the
  app init SQL must run as `supabase_admin` (local trust auth).
- Ansible's `template` writes a new inode on each edit — bind-mounted scripts
  inside the container are pinned to the original inode, so the playbook
  pipes `init/00-app.sql` over stdin instead of using `psql -f`.
