# NetBird Ansible Deployment

Deploys [NetBird](https://github.com/netbirdio/netbird) self-hosted with **Casdoor** as the OIDC IdP.
Stack runs in Docker on a single host, fronted by the host's existing nginx (TLS via Let's Encrypt).

## Pre-requisites

1. **DNS** (Cloudflare, **DNS-only / grey cloud** — gRPC and UDP do not work behind the proxy):
   - `netbird.starslab.qzz.io` → host
   - `coturn.starslab.qzz.io`  → host
2. **TLS cert** at `/etc/nginx/ssl/netbird.{crt,key}` covering both names. Issued via acme.sh DNS-01:
   ```bash
   CF_Token=... /root/.acme.sh/acme.sh --issue --dns dns_cf \
     -d netbird.starslab.qzz.io -d coturn.starslab.qzz.io --keylength ec-256
   /root/.acme.sh/acme.sh --install-cert -d netbird.starslab.qzz.io --ecc \
     --key-file /etc/nginx/ssl/netbird.key --fullchain-file /etc/nginx/ssl/netbird.crt \
     --reloadcmd "systemctl reload nginx"
   ```
3. **Casdoor OIDC application** (credentials live in sops-encrypted `secrets/netbird.yaml`).
   Edit with `sops secrets/netbird.yaml` from the repo root. Recreate the Casdoor app if needed:
   ```bash
   curl -c /tmp/cj -X POST https://casdoor.starslab.qzz.io/api/login -H 'Content-Type: application/json' \
     -d '{"organization":"built-in","application":"app-built-in","username":"admin","password":"<pw>","autoSignin":true,"type":"login"}'
   curl -b /tmp/cj -X POST https://casdoor.starslab.qzz.io/api/add-application -H 'Content-Type: application/json' \
     -d '{ "owner":"admin","name":"netbird","organization":"built-in","displayName":"NetBird",
           "clientId":"<random-hex-20>","clientSecret":"<random-hex-48>",
           "redirectUris":["https://netbird.starslab.qzz.io/auth","https://netbird.starslab.qzz.io/silent-auth",
                           "https://netbird.starslab.qzz.io/nb-auth","https://netbird.starslab.qzz.io/nb-silent-auth",
                           "http://localhost:53000","http://localhost:54000"],
           "grantTypes":["authorization_code","refresh_token","client_credentials","urn:ietf:params:oauth:grant-type:device_code"],
           "tokenFormat":"JWT","expireInHours":24,"refreshExpireInHours":168,
           "enablePassword":true,"enableSignUp":true,"enableSigninSession":true }'
   ```
4. **Open VPS firewall** for: `443/tcp` (already), `3478/udp`, `49152-65535/udp` (TURN relay range).

## Usage

```bash
cd ansible/netbird
ansible-playbook -i inventory.ini deploy-netbird.yml
```

OIDC credentials are loaded from `secrets/netbird.yaml` (sops-encrypted) at the repo root.
View / edit them with:
```bash
sops secrets/netbird.yaml
```

## Layout

| File                              | Purpose                                                |
|-----------------------------------|--------------------------------------------------------|
| `deploy-netbird.yml`              | Main playbook                                          |
| `inventory.ini`                   | Target host (`203.116.95.146`)                         |
| `vars/main.yml`                   | Domains, ports, image versions                         |
| `../../secrets/netbird.yaml`      | Casdoor client_id/secret (sops-encrypted)              |
| `templates/docker-compose.yml.j2` | dashboard + signal + relay + management + coturn       |
| `templates/management.json.j2`    | NetBird mgmt config + OIDC + DeviceAuth + PKCE         |
| `templates/turnserver.conf.j2`    | Coturn config (host networking for relay range)        |
| `templates/nginx-netbird.conf.j2` | TLS terminator with gRPC + WebSocket routing           |

## Post-deployment

1. Visit `https://netbird.starslab.qzz.io` → click **Sign in** → redirected to Casdoor.
2. Sign in (e.g. `admin/<pw>`) → first user becomes the NetBird account owner.
3. Install the NetBird agent on each device (`netbird up --management-url https://netbird.starslab.qzz.io`).
   The agent will open a browser to Casdoor for device-flow auth.

## Auth model

- NetBird's **embedded user-management** is disabled (`IdpManagerConfig.ManagerType = "none"`).
  Users authenticate via Casdoor; NetBird trusts the JWT and creates user records on first login.
  Inviting users / syncing groups must be done in Casdoor itself.
- Both **DeviceAuthorizationFlow** (CLI agents) and **PKCEAuthorizationFlow** (desktop apps) point at
  Casdoor's standard OIDC endpoints discovered from `/.well-known/openid-configuration`.
