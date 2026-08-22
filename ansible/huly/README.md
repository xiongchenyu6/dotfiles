# Huly on lubancat

Self-hosted project/knowledge platform: thirteen containers (CockroachDB,
Redpanda, Elasticsearch, MinIO and the Huly services) behind the host nginx,
with Auth0 as an OIDC provider.

| | |
|---|---|
| Public URL | https://huly.starslab.qzz.io |
| Deploy dir | `/opt/huly` |
| Local port | `127.0.0.1:8087` |
| TLS | `/etc/nginx/ssl/huly.{crt,key}` (acme.sh, Cloudflare DNS-01) |

The stack keeps the upstream's own nginx container, which owns the routing map
between `/_accounts`, `/_transactor`, `/_collaborator`, `/_rekoni`, `/_stats`
and the front end. The host nginx only terminates TLS in front of it, so that
routing table stays the upstream's to maintain.

## Prerequisites

1. DNS for `huly.starslab.qzz.io` pointing at the host.
2. A TLS cert:

   ```bash
   /root/.acme.sh/acme.sh --issue --dns dns_cf -d huly.starslab.qzz.io \
       --keylength ec-256 --server letsencrypt
   /root/.acme.sh/acme.sh --install-cert -d huly.starslab.qzz.io --ecc \
       --key-file /etc/nginx/ssl/huly.key \
       --fullchain-file /etc/nginx/ssl/huly.crt \
       --reloadcmd "systemctl reload nginx"
   ```
3. The Auth0 application from `scripts/auth0-setup.sh` (callback
   `https://huly.starslab.qzz.io/_accounts/auth/openid/callback`).

## Deploy

```bash
cd ansible/huly
ansible-playbook -i inventory.ini deploy-huly.yml
```

First boot is slow: CockroachDB initialises, Elasticsearch installs its
ingest-attachment plugin, and a dozen services wait on each other. The
playbook polls the front end for up to ten minutes.

## Resource budget

This is by far the heaviest thing on the box, which also runs Supabase,
Casdoor, NetBird, sub2api and now three other stacks in this series. Two
limits in `vars/main.yml` keep it from starving them:

- `huly_elastic_heap` / `huly_elastic_mem_limit` — a 1 GB JVM heap inside a
  1.6 GB container.
- `huly_redpanda_mem_limit` — 1 GB, with `--smp 1`.

Check `free -h` and `docker stats` after a deploy before assuming there is
room for more.

## Differences from upstream

The upstream `compose.yml` ships fixed development credentials
(`minioadmin`, `cr_secret`, `panda_secret`). This deployment generates all of
them and keeps them in `/opt/huly/.secrets.yml`; they are baked into the
databases on first boot, so the playbook reuses them rather than rotating
them on a re-run.

Upstream also publishes CockroachDB, Redpanda and hulykvs ports on the host.
Here nothing but the stack's own nginx is published, and only on loopback.

## Upgrading

Bump `huly_version` in `vars/main.yml` and re-run the playbook. Check the
upstream [MIGRATION.md](https://github.com/hcengineering/huly-selfhost/blob/main/MIGRATION.md)
first — some releases need a migration tool run.
