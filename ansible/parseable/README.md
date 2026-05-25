# Parseable on `gz-office` (183.6.107.47)

Single-binary Rust observability platform — logs, metrics, traces — with
rustfs as the S3 backend and Casdoor (on the autolife host) for OIDC SSO.

## Public URL

```
https://parseable.gz.autolife.ai:8444/
```

## What the playbook does

1. Loads `secrets/parseable.yaml` (admin pw + OIDC client creds) and
   `secrets/rustfs.yaml` (S3 access/secret key) via sops.
2. Installs `awscli` if missing, then uses it to ensure the `parseable`
   bucket exists on the local rustfs.
3. Renders `/opt/parseable/docker-compose.yml` with all the `P_*` env vars
   Parseable expects, including OIDC + S3 + admin.
4. `docker compose up -d` and waits for `/api/v1/liveness` to return 200.
5. Drops nginx vhost `signoz-gz.conf` ... ugh I mean `parseable-gz.conf`
   on port 8444 with `server_name parseable.gz.autolife.ai`.

## Prerequisites (do these once before running the playbook)

### 1. DNS

Add an A record on Cloudflare (the `autolife.ai` zone is on Cloudflare):

```
parseable.gz.autolife.ai → 183.6.107.47
```

You can leave Cloudflare proxy off (grey cloud) — the wildcard cert is
LE-issued by acme.sh on the host and TLS terminates there.

### 2. Casdoor application

On the autolife host, create a Casdoor application that matches the
`client_id` / `client_secret` in `secrets/parseable.yaml`. Easiest way is
to clone an existing app row (same pattern used for vaultwarden):

```sql
-- SSH to ubuntu@43.139.62.96, then:
sudo -u postgres psql -d casdoor

INSERT INTO application
SELECT
    owner,
    'parseable-gz'                                                       AS name,
    NOW()::text                                                          AS created_time,
    'Parseable (gz)'                                                     AS display_name,
    category, type, scopes, logo,
    'Parseable'                                                          AS title,
    favicon, "order",
    'https://parseable.gz.autolife.ai:8444'                              AS homepage_url,
    'Parseable observability platform on gz-office'                      AS description,
    organization, cert, default_group, header_html,
    enable_password, enable_sign_up, disable_signin, enable_signin_session,
    enable_auto_signin, enable_code_signin, enable_exclusive_signin,
    enable_saml_compress, enable_saml_c14n10, enable_saml_post_binding,
    disable_saml_attributes, enable_saml_assertion_signature,
    use_email_as_saml_name_id, enable_web_authn, enable_link_with_email,
    org_choice_mode, saml_reply_url, providers, signin_methods,
    signup_items, signin_items, grant_types, tags, saml_attributes,
    saml_hash_algorithm, is_shared, ip_restriction,
    '68117546e264775abf4b'                                               AS client_id,
    '5c8a20d2665dd94915884327521b6126c58e02c1'                           AS client_secret,
    client_cert,
    '["https://parseable.gz.autolife.ai:8444/api/v1/o/code"]'::text      AS redirect_uris,
    forced_redirect_origin,
    'JWT-Standard'                                                       AS token_format,
    token_signing_method, token_fields, token_attributes,
    expire_in_hours, refresh_expire_in_hours, cookie_expire_in_hours,
    signup_url, signin_url, forget_url, affiliation_url, ip_whitelist,
    terms_of_use, signup_html, signin_html, theme_data, footer_html,
    form_css, form_css_mobile, form_offset, form_side_html,
    form_background_url, form_background_url_mobile,
    failed_signin_limit, failed_signin_frozen_time, code_resend_timeout,
    custom_scopes, domain, other_domains, upstream_host, ssl_mode,
    ssl_cert, page_html, enable_guest_signin, saml_c14n_prefix,
    backchannel_logout_uri
FROM application WHERE name = 'netbird';
```

`token_format = 'JWT-Standard'` is required — same reason as vaultwarden:
strict OIDC clients reject Casdoor's default `JWT` mode (`address` claim
as `[]` instead of an object).

After the INSERT, restart Casdoor so the in-memory app cache picks it up:

```bash
systemctl --user restart casdoor
```

## Run it

```bash
cd ansible/parseable
ansible-playbook -i inventory.ini deploy-parseable.yml -K
```

`-K` prompts for the `autolife` sudo password on the gz-office host (same
convention as the rustfs playbook).

## Sending data to Parseable

OTLP endpoints (exposed via host nginx on :8444):

```
OTLP/HTTP:  https://parseable.gz.autolife.ai:8444/v1/logs
                                                   /v1/metrics
                                                   /v1/traces
```

Authenticate OTLP requests with the local admin (Basic auth) until you
generate proper per-app API tokens in the Parseable UI.

## Notes / gotchas

- **S3 backend is rustfs on the same host.** Bucket name `parseable` is
  auto-created by the playbook. Data is durable on rustfs; the local
  `/data/parseable/staging/` directory holds only hot batches before
  flush.
- **Port 8444 is shared with rustfs**, distinguished by `server_name`.
  Don't enable HTTP/2 on this vhost — rustfs needs HTTP/1.1 for browser
  concurrency caps, and nginx 1.24 sets http2 per port-socket.
- **`P_OIDC_REDIRECT_URL` is not a Parseable env var.** Parseable derives
  the redirect URL from the request's `Host` header. Make sure nginx
  forwards `Host` correctly (the template does: `$host:$server_port`).
- **OIDC user lifecycle.** First time a Casdoor user logs in, Parseable
  creates a record for them with no roles. The local admin must assign
  roles in the UI before the OIDC user can do anything.
- **Bumping the image** — edit `parseable_image` in `vars/main.yml`
  (defaults to `parseable/parseable:edge`; pin to `:vX.Y.Z` for stability).
