# AFFiNE Community Edition on lubancat

Knowledge base / whiteboard workspace. Four containers (server, one-shot
migration job, Postgres with pgvector, Redis) behind the host nginx, with
Auth0 as an OIDC provider.

| | |
|---|---|
| Public URL | https://affine.starslab.qzz.io |
| Admin UI | https://affine.starslab.qzz.io/admin |
| Deploy dir | `/opt/affine` |
| Local port | `127.0.0.1:3010` |
| TLS | `/etc/nginx/ssl/affine.{crt,key}` (acme.sh, Cloudflare DNS-01) |

## Prerequisites

1. DNS for `affine.starslab.qzz.io` pointing at the host.
2. A TLS cert:

   ```bash
   /root/.acme.sh/acme.sh --issue --dns dns_cf -d affine.starslab.qzz.io \
       --keylength ec-256 --server letsencrypt
   /root/.acme.sh/acme.sh --install-cert -d affine.starslab.qzz.io --ecc \
       --key-file /etc/nginx/ssl/affine.key \
       --fullchain-file /etc/nginx/ssl/affine.crt \
       --reloadcmd "systemctl reload nginx"
   ```
3. The Auth0 application from `scripts/auth0-setup.sh` (callback
   `https://affine.starslab.qzz.io/oauth/callback`).

## Seeding the image

ghcr.io serves this package to lubancat at a few KB/s when the object is cold
in the CDN edge in front of it — a first pull takes hours, while the same blob
from a machine with a warm edge runs at megabytes a second. Authenticating to
ghcr.io does not change it.

If a cold deploy stalls on the pull, seed the image from a machine that can
reach ghcr.io at a normal speed:

```bash
docker pull --platform linux/arm64 ghcr.io/toeverything/affine:stable
docker save ghcr.io/toeverything/affine:stable | gzip -1 \
    | ssh lubancat 'cat > /tmp/affine-image.tar.gz'
ssh lubancat 'sudo sh -c "gunzip -c /tmp/affine-image.tar.gz | docker load"'
```

Then re-run the playbook; `docker compose pull` finds the tag already present.

## Deploy

```bash
cd ansible/affine
ansible-playbook -i inventory.ini deploy-affine.yml
```

Open `/admin` once afterwards to create the initial owner account — AFFiNE
has no first-run account of its own.

## Configuration

Everything the server reads lives in `/opt/affine/config/config.json`,
rendered from `templates/config.json.j2`. Anything edited through the admin
panel is stored in the database and will be overwritten by the next
deploy for keys the template owns, so make lasting changes in
`vars/main.yml`.

Two settings worth knowing:

- `affine_allow_signup: false` — password signup is closed, because Auth0 is
  the intended way in.
- `affine_allow_signup_for_oauth: true` — the first Auth0 login still
  provisions an account. Set it to `false` once everyone who needs access has
  logged in and the workspace should be sealed.

Full-text search runs on AFFiNE's embedded indexer rather than
Elasticsearch — Huly already runs one JVM on this box and a second would not
fit.
