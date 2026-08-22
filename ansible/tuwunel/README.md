# Tuwunel (Matrix homeserver) on lubancat

A Rust Matrix homeserver, the successor to conduwuit. Runs as a single
container behind the host nginx; Auth0 is the identity provider.

| | |
|---|---|
| `server_name` | `starslab.qzz.io` (MXIDs are `@user:starslab.qzz.io`) |
| Homeserver | https://matrix.starslab.qzz.io |
| Deploy dir | `/opt/tuwunel` |
| Local port | `127.0.0.1:8008` |
| TLS | `/etc/nginx/ssl/tuwunel.{crt,key}` (acme.sh, Cloudflare DNS-01) |

`server_name` is baked into every MXID and room ID ever created here. It can
never be changed without abandoning the server's whole history.

## Apex delegation

The name in the MXID and the host actually running the homeserver are
deliberately different: `server_name` is the apex so identifiers read
`@you:starslab.qzz.io`, while the server answers on
`matrix.starslab.qzz.io`. Clients and other homeservers bridge the two by
reading two documents from the apex:

```
https://starslab.qzz.io/.well-known/matrix/client
https://starslab.qzz.io/.well-known/matrix/server
```

Tuwunel serves both on its own vhost (`[global.well_known]` in
`tuwunel.toml`), but a client resolving `starslab.qzz.io` looks for them on the
apex — which is a Cloudflare Worker (`quant`) bound as a **custom domain**,
serving a site this repository does not build.

So the delegation is published by a second Worker, `well-known-worker/`, on a
route scoped to that one path prefix:

```
starslab.qzz.io/.well-known/matrix/*  ->  matrix-well-known
```

A route pattern is more specific than the apex custom domain and wins for
those paths; everything else on the apex still reaches the site untouched.
Deploy it with:

```bash
./scripts/cloudflare-matrix-delegation.sh --dry-run   # resolve the account only
./scripts/cloudflare-matrix-delegation.sh
```

It authenticates with the `wrangler` OAuth login on the machine — `wrangler
login` refreshes it when the access token has expired. The DNS-01 token in
`secrets/common.yaml` cannot do this; it only carries *Zone -> DNS -> Edit*.

Keep `HOMESERVER` in `well-known-worker/wrangler.toml` in sync with
`tuwunel_host_name`.

Verify end to end — this is the check that actually proves federation, not
just that the documents parse:

```bash
curl -s "https://federationtester.matrix.org/api/report?server_name=starslab.qzz.io" \
    | jq '{FederationOK, WellKnownResult: .WellKnownResult["m.server"], Version}'
```

## Prerequisites

1. A DNS record for `matrix.starslab.qzz.io` pointing at the host (proxied
   through Cloudflare, like the rest of the zone), plus the apex redirect rule
   described above.
2. A TLS cert. The token acme.sh needs is already in
   `/root/.acme.sh/account.conf`:

   ```bash
   /root/.acme.sh/acme.sh --issue --dns dns_cf -d matrix.starslab.qzz.io \
       --keylength ec-256 --server letsencrypt
   /root/.acme.sh/acme.sh --install-cert -d matrix.starslab.qzz.io --ecc \
       --key-file /etc/nginx/ssl/tuwunel.key \
       --fullchain-file /etc/nginx/ssl/tuwunel.crt \
       --reloadcmd "systemctl reload nginx"
   ```
3. The Auth0 application, registered by `scripts/auth0-setup.sh`. Without it
   the playbook still deploys, just without SSO.

## Deploy

```bash
cd ansible/tuwunel
ansible-playbook -i inventory.ini deploy-tuwunel.yml
```

## Auth0

`scripts/auth0-setup.sh` creates all four applications. One trap is worth
knowing: a client-credentials request that omits `scope` gets a token carrying
only a small default subset of the grant, so `/api/v2/clients` answers 403 even
when the machine-to-machine application holds every client scope. The script
asks for them explicitly.

It creates a regular web application named `Tuwunel (Matrix)` whose callback is

```
https://matrix.starslab.qzz.io/_matrix/client/unstable/login/sso/callback/<client_id>
```

Tuwunel mandates exactly that path shape, with its own `client_id` as the last
segment — which is why the setup script creates the application first and
patches the callback afterwards.

The provider is configured `trusted = true`: Auth0 is self-operated, so a
claim that matches an existing local account is allowed to bind to it rather
than being pushed onto a freshly generated username.

## Registration

`allow_registration` is on but gated behind a token, so the SSO flow can
provision an account on first login while the plain registration endpoint
stays closed to the internet. The token is in `/opt/tuwunel/.secrets.yml`.

## Notes

- `/.well-known/matrix/server` does double duty: it names the delegate host
  and moves federation onto 443, because the Cloudflare edge in front of this
  zone only proxies 443 — not the 8448 the specification defaults to.
- Media uploads are capped at 90 MiB, under Cloudflare's free-plan 100 MB
  request body limit, so a too-large upload fails in the client with a real
  error instead of at the edge.
- The client IP is read from `CF-Connecting-IP`; nothing else survives the
  edge intact.

## Operating

```bash
ssh lubancat
sudo docker logs -f tuwunel
sudo docker exec -it tuwunel tuwunel --help
# Admin commands go through the admin room the server creates on first start.
```
