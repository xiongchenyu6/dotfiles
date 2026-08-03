# sing-box deployment

This directory deploys the same encrypted Shadowsocks service to two different
Linux platforms:

| Inventory host | Platform | Release asset | Privilege path |
| --- | --- | --- | --- |
| `lubancat` | Ubuntu 24.04 / ARM64 | `linux-arm64` | SSH key plus the existing SOPS-encrypted sudo password |
| `sg-office` | NixOS / AMD64 | `linux-amd64` | Existing root SSH key |

The old `ubuntu-server` entry remains in the `singbox_legacy` inventory group and
is not targeted by this playbook.

## What the playbook guarantees

- Reads the Shadowsocks method, password, and port from
  `secrets/servers.yaml`; decrypted values are never written to the repository
  or printed by Ansible.
- Reads only `lubancat.become_password` from the repository-level
  `../../secrets/common.yaml`. The legacy server SSH password is not applied to
  either new host.
- Pins the current stable sing-box 1.13.14 and verifies the official AMD64/ARM64 archives against
  the SHA-256 values in the playbook.
- Runs `sing-box check` before a configuration can replace the live file and
  again after activation.
- Accepts both native Shadowsocks TCP/UDP traffic and optional multiplexed
  client connections without requiring multiplex padding. This avoids padding
  overhead and keeps non-multiplexed clients compatible.
- Installs a hardened, unprivileged systemd service and verifies both its active
  state and its local listener.
- Leaves UFW, the NixOS firewall, interfaces, routes, NetworkManager, WireGuard,
  Docker, and Podman untouched.
- Captures existing binary/config/unit files before replacement. If activation
  or validation fails, the play rolls those files and the prior service state
  back automatically.

On NixOS, `/etc/systemd/system` points into the read-only Nix store. The playbook
therefore uses the standard local-unit path
`/usr/local/lib/systemd/system/sing-box.service` and a persistent
`multi-user.target.wants` link in the same unit search path. `deploy.sh`
discovers an already-present Nix-store Python interpreter without changing the
Nix profile.

## Run it

From any directory:

```bash
~/dotfiles/ansible/sing-box/deploy.sh
```

Deploy one host only:

```bash
~/dotfiles/ansible/sing-box/deploy.sh --limit lubancat
~/dotfiles/ansible/sing-box/deploy.sh --limit sg-office
```

Required controller tools are `ansible-playbook`, `sops`, the
`community.sops` collection, and access to the existing decryption key.
Host-key checking is enabled, so both targets must already have trusted
`known_hosts` entries.

## Non-secret verification

```bash
ssh lubancat 'systemctl is-active sing-box'
ssh root@sg-office 'systemctl is-active sing-box'
ssh root@sg-office \
  'systemctl list-dependencies --plain multi-user.target | grep sing-box'
```

Configuration contents and decrypted credentials should not be copied into
logs, tickets, or command-line arguments.

Official upstream references:

- <https://sing-box.sagernet.org/installation/package-manager/>
- <https://github.com/SagerNet/sing-box/releases/tag/v1.13.14>
