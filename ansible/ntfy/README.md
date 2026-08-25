# ntfy on lubancat

Push notification server, behind the host nginx, on its own user database and
per-topic ACLs.

| | |
|---|---|
| Public URL | https://ntfy.starslab.qzz.io |
| Deploy dir | `/opt/ntfy` |
| Local port | `127.0.0.1:8090` |
| TLS | `/etc/nginx/ssl/ntfy.{crt,key}` (acme.sh, Cloudflare DNS-01) |

## Why there is no Auth0 in front of this one

Every other service in this stack signs in through Auth0. ntfy does not, and
that is deliberate.

ntfy has no OIDC support: it authenticates against its own user database and
per-topic ACLs, and its clients — the Android and iOS apps, `curl` publishers,
webhooks, UnifiedPush distributors — can only present an ntfy token or HTTP
Basic credentials. None of them can run an interactive browser login.

The obvious workaround is to front it with an auth proxy (oauth2-proxy) and let
API clients skip the proxy when they already carry a credential. That was tried
here and removed, because the escape hatch guts the gate:

```console
$ curl -o /dev/null -w '%{http_code}\n' https://ntfy.starslab.qzz.io/v1/account
302                      # no credential -> bounced to Auth0

$ curl -o /dev/null -w '%{http_code}\n' \
      -H 'Authorization: Bearer anything-at-all' \
      https://ntfy.starslab.qzz.io/v1/account
401                      # any header at all -> straight through to ntfy
```

The proxy can only tell whether a credential is *present*, not whether it is
valid, so one junk header walks past it. What remained protected was the set of
requests carrying no credential — the login page and static assets — which is
close to nothing. Against that it cost an extra container, an `if` nested in an
nginx `location`, two separate logins for a browser user whose Auth0 identity
was never mapped to an ntfy one, and a confusing failure mode where a plain
webhook received an HTML redirect instead of ntfy's 401.

Removing the escape hatch would make the gate real and break every API client,
which is unacceptable for a push service.

So ntfy keeps the authentication it was designed around: `auth-default-access:
deny-all`, named users, per-topic ACLs and bearer tokens. Nothing is readable or
publishable without an ntfy credential, which is the same guarantee the proxy
was nominally there to provide.

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

## Deploy

```bash
cd ansible/ntfy
ansible-playbook -i inventory.ini deploy-ntfy.yml
```

The playbook creates an `admin` user on first run; its password lands in
`/opt/ntfy/.secrets.yml`.

## Users and topics

There is no self-service signup and no SSO, so the roster is provisioned. It is
declared in the repository's sops store under `ntfy.users` and rendered into
`server.yml` as ntfy's `auth-users` / `auth-access` entries — nothing is typed
into the auth database by hand, so a rebuilt host comes back with the same
people on it.

The roster is encrypted rather than sitting in `vars/main.yml` because this
repository is public and the roster names colleagues.

```bash
./scripts/ntfy-user.sh list
./scripts/ntfy-user.sh add alice --access 'alerts-*:ro' --access 'deploys:rw'
./scripts/ntfy-user.sh add ops --admin
./scripts/ntfy-user.sh passwd alice        # rotate, prints a new password once
./scripts/ntfy-user.sh remove alice

# apply
(cd ansible/ntfy && ansible-playbook -i inventory.ini deploy-ntfy.yml)
```

`add` and `passwd` generate a 20-character password, hash it with `ntfy user
hash` on the server, store only the bcrypt hash, and print the password once
for handover.

Permissions are `rw`, `ro`, `wo` or `deny`, and the topic may use a `*`
wildcard (`alerts-*`). An `--admin` user has read-write access to everything
and needs no per-topic entries. `auth-default-access` stays `deny-all`, so a
user sees exactly the topics granted to them and an anonymous request gets a
403.

### Machines that publish

A service that only posts notifications should get its own write-only user
rather than sharing a person's credentials:

```bash
./scripts/ntfy-user.sh add ci-bot --access 'deploys:wo'
ssh lubancat 'docker exec -it ntfy ntfy token add ci-bot'    # -> tk_...
```

```bash
curl -H "Authorization: Bearer tk_..." -d "build 412 deployed" \
    https://ntfy.starslab.qzz.io/deploys
```

Tokens live in the auth database rather than the config, because they are
issued and revoked per device.

### UnifiedPush for Matrix

`up*` is the one topic prefix anonymous clients may write to, granted in
`vars/main.yml` as `everyone:up*:wo`. That exists so Tuwunel can drive Android
Matrix push through this server; the reasoning and the boundary tests are in
[`../tuwunel/README.md`](../tuwunel/README.md).

### Clients

The Android and iOS apps, and the web app at the URL above, all take a server
URL plus username and password. `require-login` is on, so the web app opens
straight at the login form.

