# panda.qzz.io — Supabase-style backend prompt

Paste this whole section into any AI / agent prompt that needs to consume the
self-hosted Supabase stack running on `oracle-arm-002` (Oracle Cloud
aarch64-linux, 138.2.76.211). All endpoints work today; last verified
2026-04-23.

---

## What you have access to

| Surface | URL / DSN | Auth | Notes |
|---|---|---|---|
| **PostgREST (HTTP API)** | `https://api.panda.qzz.io` | Anonymous by default (`anon` role). Pass `Authorization: Bearer <JWT>` to upgrade to `authenticated` or `service_role`. | Exposes schema `api`. Cloudflare-proxied. 100 s idle cap on Free plan. |
| **GoTrue auth** | `https://auth.panda.qzz.io` | — | Endpoints: `/signup`, `/token?grant_type=password`, `/logout`, `/settings`, `/recover`, `/verify`. `GOTRUE_MAILER_AUTOCONFIRM=true` so new users get an access_token immediately from `/signup`. CF-proxied. |
| **Supabase Realtime** | `wss://<external_id>.realtime.panda.qzz.io/socket/websocket?apikey=<JWT>&vsn=1.0.0` | Tenant JWT (same `JWT_SECRET` as the auth layer) | Per-tenant via Host header subdomain. Admin API at `https://realtime.panda.qzz.io/api/tenants` (role=supabase_admin JWT). Wildcard `*.realtime.panda.qzz.io` is DNS-only → direct origin (still TLS). |
| **Direct PostgreSQL** | `postgres://quant:<pw>@db.panda.qzz.io:5432/api?sslmode=require` | Password — ask the operator | DNS-only (CF proxy doesn't tunnel 5432). `sslmode=require` works; `verify-full` needs the self-signed origin cert (CN=`db.panda.qzz.io`). |

## Database

- **Database:** `api` on PostgreSQL 18 (aarch64).
- **Roles (Supabase convention):**
  - `anon` — unauthenticated PostgREST caller, `statement_timeout=3s`
  - `authenticated` — any logged-in user
  - `service_role` — bypass RLS
  - `api_authenticator` — PostgREST's DB login (inherits the above)
  - `supabase_auth_admin` — owns the `auth` schema
  - `supabase_admin` — superuser + REPLICATION (used by realtime + migrations)
  - `quant` — public SQL access over TLS (granted CONNECT; use schema `quant`)
- **Schemas on default search_path:** `public`, `extensions`
- **30 extensions installed.** Commonly used: `timescaledb`, `pgvector` (+ `pgvectorscale`), `pg_graphql`, `pg_cron`, `pg_net`, `pgsodium`, `pgmq`, `pgjwt`, `pgtap`, `postgis`, `plv8`, `pg_tle`, `pg_stat_statements`, `pg_repack`, `pg_hint_plan`, `http` (pgsql-http), `rum`, `hstore`, `citext`, `ltree`, `uuid-ossp`, `pgcrypto`, `btree_gin/gist`, `pg_hashids`, `hypopg`, `index_advisor`. `plan_filter` is preloaded (use the `plan_filter.statement_cost_limit` GUC — no `CREATE EXTENSION` needed).
- **Realtime publication:** `supabase_realtime` (empty by default — `ALTER PUBLICATION supabase_realtime ADD TABLE api.<mytable>` to stream row changes to WS subscribers).
- **Logical replication:** `wal_level=logical`, 10 slots, 10 wal_senders.

## How to do things

### Sign up / log in (gotrue)

```bash
AUTH=https://auth.panda.qzz.io

# signup — returns access_token because autoconfirm is on
curl -X POST "$AUTH/signup" \
  -H 'Content-Type: application/json' \
  -d '{"email":"alice@example.com","password":"CorrectHorseBattery!"}'

# password login
curl -X POST "$AUTH/token?grant_type=password" \
  -H 'Content-Type: application/json' \
  -d '{"email":"alice@example.com","password":"CorrectHorseBattery!"}'
# -> { "access_token": "...", "refresh_token": "...", "user": {...} }
```

JWT claims: `sub` (uuid), `email`, `aud=authenticated`, `role=authenticated`, `exp` (1 hr).

### CRUD via PostgREST

Anonymous read of table exposed in schema `api`:

```bash
curl https://api.panda.qzz.io/items
curl 'https://api.panda.qzz.io/items?id=eq.123'
```

Authenticated write (must use the JWT from `/token`):

```bash
curl -X POST https://api.panda.qzz.io/items \
  -H "Authorization: Bearer $JWT" \
  -H 'Content-Type: application/json' \
  -H 'Prefer: return=representation' \
  -d '{"name":"hello"}'
```

Filters / ordering / select columns are PostgREST standard (see https://postgrest.org/).

### Subscribe to row changes (realtime)

One-time per client: provision the tenant via Admin API. Uses a JWT
signed with `JWT_SECRET` and `role=supabase_admin`.

```bash
# 1) Create tenant (per-client-app, pick a stable external_id).
curl -X POST https://realtime.panda.qzz.io/api/tenants \
  -H "Authorization: Bearer $ADMIN_JWT" \
  -H 'Content-Type: application/json' \
  -d '{"tenant":{"name":"myapp","external_id":"myapp","jwt_secret":"<same as JWT_SECRET>",
       "extensions":[{"type":"postgres_cdc_rls","settings":{
         "db_name":"api","db_host":"127.0.0.1","db_port":"5432",
         "db_user":"supabase_admin","db_password":"<DB_PASSWORD>",
         "region":"us-east-1","publication":"supabase_realtime",
         "poll_interval_ms":100,"poll_max_record_bytes":1048576,
         "slot_name":"supabase_realtime_myapp"}}]}}'

# 2) Add the table you want to stream to the publication (one-time, psql).
psql "host=db.panda.qzz.io user=quant dbname=api sslmode=require" \
  -c "ALTER PUBLICATION supabase_realtime ADD TABLE api.items"

# 3) Subscribe over WS at <external_id>.realtime.panda.qzz.io.
# JS client: https://github.com/supabase/realtime-js
wscat -c "wss://myapp.realtime.panda.qzz.io/socket/websocket?apikey=$USER_JWT&vsn=1.0.0"
# Phoenix Channels join frame:
# {"topic":"realtime:public:items","event":"phx_join","payload":{"config":{"postgres_changes":[{"event":"*","schema":"api","table":"items"}]}},"ref":"1"}
```

Note: tenant subdomains are DNS-only (`*.realtime.panda.qzz.io` → 138.2.76.211 directly). A single TLS cert (LE) covers both apex and wildcard.

### Direct SQL / analytics (quant)

```bash
psql "host=db.panda.qzz.io user=quant dbname=api sslmode=require"
# Your playground schema is `quant`, which is on your search_path.
# You also have CONNECT on `api`; explicit grants are needed to read app tables.
```

TimescaleDB, pgvector, and friends are all available from here:

```sql
-- time-series
CREATE TABLE quant.ticks (time timestamptz, sym text, price float);
SELECT create_hypertable('quant.ticks', 'time', chunk_time_interval => INTERVAL '1 hour');
SELECT time_bucket('5 minutes', time) AS bucket, sym, avg(price)
  FROM quant.ticks GROUP BY bucket, sym;

-- vector similarity
CREATE TABLE quant.docs (id uuid PRIMARY KEY, embedding vector(1536));
SELECT id FROM quant.docs ORDER BY embedding <=> '[0.1,0.2,...]'::vector LIMIT 5;

-- index suggestions
SELECT * FROM extensions.index_advisor('SELECT * FROM api.items WHERE tenant_id = 3');
```

## Secrets you'll need to ask the operator for

Only the operator can hand out these. Don't try to discover them yourself.

- `JWT_SECRET` — shared between gotrue, postgrest, and realtime tenants. Used to mint and verify every JWT in the stack.
- `ADMIN_JWT` — a JWT signed with `JWT_SECRET` and `role=supabase_admin` (short-lived, generate on demand).
- `DB_PASSWORD` — password for `supabase_admin` when creating a realtime tenant. Same password is used for `api_authenticator` / `supabase_auth_admin` internally (one secret, many roles).
- `QUANT_PASSWORD` — for direct PG access as the `quant` role.

Generating `ADMIN_JWT` (HS256):

```bash
b64url() { openssl base64 -A | tr '+/' '-_' | tr -d '='; }
H=$(printf '{"alg":"HS256","typ":"JWT"}' | b64url)
P=$(printf '{"role":"supabase_admin","iss":"supabase","iat":%d,"exp":%d}' \
      "$(date +%s)" "$(($(date +%s)+600))" | b64url)
S=$(printf "$H.$P" | openssl dgst -sha256 -hmac "$JWT_SECRET" -binary | b64url)
ADMIN_JWT="$H.$P.$S"
```

## Known limits / gotchas

- **CF Free tier:** 100 MB upload cap, 100 s idle connection cap. Put large uploads on a separate DNS-only subdomain (like `db`), or move off Free. CF does not proxy ports other than HTTP/HTTPS — anything on 5432, 6379, etc. must resolve to a DNS-only record.
- **Realtime tenant routing is via Host header**. `curl` against a proxied `apex.realtime.panda.qzz.io` will hit tenant "apex", which almost certainly doesn't exist and returns 400. Always use `<external_id>.realtime.panda.qzz.io`.
- **pgaudit is not installed** — nixpkgs marks it broken for PG18 (2026-04). Re-add once upstream fixes.
- **pg_jsonschema, wrappers, supabase_vault are not installed** — these are pgrx-based and haven't been packaged yet. If you need JSON schema validation in RLS policies, use `pg_graphql` validations or a plpgsql helper.
- **Single-node only.** Realtime has `DNS_NODES=""`, `RUN_JANITOR=true`. If you scale out, also provision a shared Erlang cluster.
- **`auth.uid()`, `auth.role()`, `auth.jwt()`** are gotrue-managed. Our RLS helpers must reference them via `SELECT auth.uid()` inside policies; EXECUTE is granted to `anon`/`authenticated`/`service_role`.

## If something is broken

- **401 from gotrue on a valid JWT:** the signing key drifted. Compare what you used vs `/run/secrets/oracle-arm-002/jwt-secret` on the host.
- **Realtime 400 on a WS upgrade:** tenant doesn't exist, or Host header extraction saw a non-tenant label. Re-provision tenant or check `SELECT external_id FROM _realtime.tenants`.
- **PostgREST 500:** usually missing schema cache reload after a DDL. Run `NOTIFY pgrst, 'reload schema'` in psql.
- **ACME cert marked self-signed:** `acme-order-renew-<host>.service` failed. Wildcards **must** use DNS-01; if you add a new cert ensure it's declared in `security.acme.certs` (not via `nginx.virtualHosts.<name>.enableACME = true`, which is HTTP-01 only).
- **Nginx 400 across all vhosts:** usually "could not build optimal proxy_headers_hash" in the error log — bump `proxy_headers_hash_{max_size,bucket_size}` in `nixos-modules/mixins-nginx.nix`.
