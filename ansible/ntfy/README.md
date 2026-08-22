# ntfy on lubancat

Push notification server. Runs behind the host nginx with an oauth2-proxy
sidecar that gates the browser surface on Auth0.

| | |
|---|---|
| Public URL | https://ntfy.starslab.qzz.io |
| Deploy dir | `/opt/ntfy` |
| Local ports | ntfy `127.0.0.1:8090`, oauth2-proxy `127.0.0.1:4180` |
| TLS | `/etc/nginx/ssl/ntfy.{crt,key}` (acme.sh, Cloudflare DNS-01) |

## How Auth0 fits

ntfy has no OIDC support of its own — it authenticates against its own user
database and per-topic ACLs, and its mobile and CLI clients can only present
an ntfy token or Basic credentials. So Auth0 sits *in front* rather than
inside:

- **Browser traffic** carries no ntfy credential, so nginx sends the
  `auth_request` subrequest to oauth2-proxy and an unauthenticated visitor is
  bounced to Auth0. The anonymous web surface is closed to everyone outside
  the tenant.
- **API traffic** — a `Authorization: Bearer tk_…` header, HTTP Basic, or the
  `?auth=` query parameter ntfy's EventSource clients use — skips the gate,
  because those clients cannot run an interactive login. They are left to
  ntfy's own authentication.

The consequence worth being explicit about: anyone can bypass the Auth0 gate
by sending an arbitrary `Authorization` header, and will then face ntfy's
`deny-all` default and be rejected. The gate hardens the browser surface; the
ntfy user database is still what actually protects topics. A browser user
therefore signs in twice — once to Auth0, once to ntfy.

Set `ntfy_oauth2_proxy_enabled: false` in `vars/main.yml` to drop the gate and
run on ntfy's authentication alone.

## Prerequisites

1. DNS for `ntfy.starslab.qzz.io` pointing at the host.
2. A TLS cert:

   ```bash
   /root/.acme.sh/acme.sh --issue --dns dns_cf -d ntfy.starslab.qzz.io \
       --keylength ec-256 --server letsencrypt
   /root/.acme.sh/acme.sh --install-cert -d ntfy.starslab.qzz.io --ecc \
       --key-file /etc/nginx/ssl/ntfy.key \
       --fullchain-file /etc/nginx/ssl/ntfy.crt \
       --reloadcmd "systemctl reload nginx"
   ```
3. The Auth0 application from `scripts/auth0-setup.sh` (callback
   `https://ntfy.starslab.qzz.io/oauth2/callback`). Without it the playbook
   deploys ntfy without the gate.

## Deploy

```bash
cd ansible/ntfy
ansible-playbook -i inventory.ini deploy-ntfy.yml
```

The playbook creates an `admin` user on first run; its password lands in
`/opt/ntfy/.secrets.yml`.

## Users and topics

```bash
ssh lubancat
sudo docker exec -it ntfy ntfy user add phone            # a publishing client
sudo docker exec -it ntfy ntfy access phone "alerts" rw
sudo docker exec -it ntfy ntfy token add phone           # -> tk_…
```

Publish with that token:

```bash
curl -H "Authorization: Bearer tk_…" -d "hello" https://ntfy.starslab.qzz.io/alerts
```
