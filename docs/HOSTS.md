# Host Inventory — `nixos-configurations/`

What each entry under `nixos-configurations/` actually installs. Facts are taken
directly from the config files; anything not declared there is not claimed here.

Two kinds of entries live in this directory:

- **Deployable hosts** (8) — registered in `flake.nix` → `ezConfigs.nixos.hosts`,
  built with `nixos-rebuild switch --flake .#<hostname>`. Both `root` and
  `freeman.xiong` home-manager profiles are wired onto them.
- **Templates / image builders** (4) — auto-discovered by ez-configs but not
  deploy targets. They exist to build images or to be imported by other hosts.

Every NixOS host also inherits a common baseline from
[`nixos-modules/default.nix`](../nixos-modules/default.nix): sops-nix, home-manager,
NUR overlays, impermanence, nix-topology, srvos common modules (sshd hardening,
update-diff on boot), plus the `kernel`, `security`, and `ssh-harden` modules,
zram swap, nftables firewall, and systemd-resolved.

## Quick reference

| Host | Platform | Role | Highlights |
|---|---|---|---|
| `game` | x86_64-linux | Gaming desktop (Legion 16ACH6h) | niri desktop, NVIDIA + VFIO passthrough, Sunshine, Waydroid, sing-box travel VPN, Hashtopolis agent, DNF private server, codexpro tunnel |
| `office` | x86_64-linux | Work laptop (Dell Latitude 5520) | niri desktop, PostgreSQL 17, corp tooling (Falcon, Fortinet VPN), mesh router |
| `autolife-robot-260` | x86_64-linux | Robot onboard host | Minimal bootstrap: sshd + NetworkManager only |
| `tcloud` | x86_64-linux | Tencent Cloud VPS | DN42 border router, coturn TURN, deepseek-free-api container, HE.net IPv6 tunnel |
| `oracle-amd-001` | x86_64-linux | Oracle Cloud | hermes-agent Telegram bot, sing-box hysteria2 inbound |
| `oracle-amd-002` | x86_64-linux | Oracle Cloud, mesh hub | WireGuard hub, IB Gateway (paper) container, hysteria2 inbound, s3fs docs mount |
| `oracle-arm-001` | aarch64-linux | Oracle Cloud, mostly idle | Base server profile, Node.js 24 |
| `oracle-arm-002` | aarch64-linux | App & data workhorse | Supabase-style stack (PostgREST/Realtime/TimescaleDB), Dify, Hashtopolis server, NautilusTrader quant stack |
| `iso` | x86_64-linux | Image builder | GNOME Calamares installer ISO (`.#packages.x86_64-linux.iso`) |
| `digitalocean` | x86_64-linux | Image builder | DigitalOcean disk image |
| `generic-nixos` | x86_64-linux | Template | Generic QEMU/SBC guest baseline |
| `digital` | x86_64-linux | Droplet config snapshot | DigitalOcean droplet w/ static networking + do-agent |

---

## Deployable hosts

### `game` — gaming desktop

Lenovo Legion 16ACH6h (hybrid graphics), tagged `nvidia,gui`.

**Desktop & gaming**
- Full GUI stack: `gui` + `wayland` (niri compositor) + `greetd`; VR module enabled.
- NVIDIA: latest driver, dynamic boost, container toolkit; patched
  `nvidia-vaapi-driver` overlay.
- Gaming runtime: gamescope (with `capSysNice`), GameMode, MangoHud perf overlay,
  ydotool input injection.
- Sunshine game-streaming host with NVENC fix (`LD_LIBRARY_PATH` to opengl-driver).
- Waydroid Android container, with a pre-start property-refresh workaround for NVIDIA.
- WeChat with HiDPI scaling wrapper + desktop entry.

**Services**
- **sing-box travel VPN** — opt-in TUN proxy (started via `pon`, stopped via `poff`;
  polkit rule lets the user toggle it password-free). Five Hysteria2 nodes with
  urltest failover, CN split-routing via local geoip/geosite rule sets, Clash API +
  metacubexd dashboard on `127.0.0.1:9090`.
- **Hashtopolis agent** — CPU+GPU hashcat client for
  `hashtopolis.panda.qzz.io`, capped at 8 GB RAM / 100% CPU quota.
- **codexpro MCP server** — lets ChatGPT read/edit repos under `~/Documents`;
  exposed publicly only through a Cloudflare named tunnel at
  `codexpro.panda.qzz.io`.
- **DNF (Dungeon Fighter Online) private server** — Docker-based (`mysql` +
  `dnf-1` containers), deliberately not auto-started at boot; managed through
  `dnf-*` shell aliases.
- PostgreSQL 18 JIT with postgis, pg_repack, pg_cron.
- NetBird client; cloudflare-warp package available.

**Networking**
- WireGuard peer `wg_ora` of the oracle-amd-002 hub; BIRD2 inner-zone router
  (`mylib.bird2-inner-config`) + babeld over the mesh; DN42 module.
- libvirt virtualization with VFIO/GPU-passthrough initrd modules; trusted
  virbr interfaces; binfmt emulation for aarch64.

**Reliability**
- Hardware watchdog (`watchdogd`) with load-average/memory thresholds that
  trigger reboot; sysctls turn hangs/oopses into panics with 10 s auto-reboot.
- Secure boot via lanzaboote; `acpi_call` module to clear Lenovo's persistent
  camera-disable EC flag; AMD watchdog driver tweaks.

---

### `office` — work laptop

Dell Latitude 5520 (`nixos-hardware` module), tagged `gui`.

**Desktop**
- Same GUI stack as `game`: `gui` + `wayland` (niri) + `greetd`, Dvorak layout.
- Laptop tuning: TLP power management, deep suspend, i915 PSR, NVMe ACPI quirk,
  lid-switch policy, performance governor, latest kernel, zram.

**Corp integration**
- CrowdStrike `falcon-sensor`.
- `openfortivpn-config` (Fortinet corporate VPN client config).

**Services & networking**
- PostgreSQL 17 JIT (postgis, pg_repack, pg_cron with background workers).
- WireGuard `wg_ora` mesh peer + BIRD2 inner router + babeld; DN42; NetBird client.
- NFS4 filesystem support; binfmt aarch64 emulation; IP forwarding enabled.

---

### `autolife-robot-260` — robot onboard host

Deliberately minimal bootstrap config:

- Disko-managed disks, GRUB with removable EFI, OpenSSH, NetworkManager.
- Only `curl` and `gitMinimal` as system packages.
- `root` + `freeman.xiong` home-manager profiles; nothing else.

---

### `tcloud` — Tencent Cloud VPS

Domain `panda.qzz.io`. The network/routing node of the fleet.

**Routing**
- DN42 border router: `bird-border` (with bird-lg looking glass) + `bird-inner`
  + babeld redistribution of `172.20.0.0/14`.
- Hurricane Electric IPv6 tunnel (`sit he-ipv6`, `2001:470:35:606::2/64`).

**Services**
- **coturn** TURN/STUN server — long-term credentials injected from sops at
  runtime; realm pinned to `tcloud.autolife.ai`.
- **deepseek-free-api** OCI container published on port 8000.
- PostgreSQL (default package) with a `freeman.xiong` superuser database.
- SSH host certificates: three host key types + `TrustedUserCAKeys` CA trust.
- NAT gateway for VPN clients behind `tun0` (UDP 1194 open).
- Datadog agent; nginx mixin; ACME wildcard certs; srvos server baseline;
  rust-motd login banner (from the shared `server` module).

---

### `oracle-amd-001` — Oracle Cloud x86 (light duty)

- **hermes-agent** (NousResearch) — Telegram bot backed by Xiaomi's MiMo model
  (`mimo-v2.5-pro`), external custom-skills directory, secrets via sops env template.
- **sing-box hysteria2 inbound** (`my.sing-box-hysteria2`, UDP 8443) — egress
  endpoint for cross-border clients (replaced earlier VLESS/VLESS+Reality setups).
- `cloudflared` installed with tunnel credentials in sops.
- BBR congestion control + enlarged TCP autotuning buffers; nginx mixin; ACME;
  core+server tiers.

---

### `oracle-amd-002` — Oracle Cloud x86 (mesh hub + quant sidecar)

The WireGuard hub every other machine peers with.

**Networking**
- WireGuard hub `wg0` (listen port 22616): peers = office, game, and three more
  clients; NAT (v4+v6) for VPN clients; multicast enabled for mDNS/babel.
- Second tunnel `wg_kioubit` to Kioubit (`hk1.g-load.eu`) carrying DN42/ULA ranges.
- BBR + large TCP buffers; IP forwarding.

**Services**
- **IB Gateway (paper trading)** — Podman container
  (`ghcr.io/gnzsnz/ib-gateway:stable` bundling Gateway + IBC + Xvfb), tuned heap
  for the small instance, daily self-restart outside US market hours. Paper API
  published on the mesh address only: `172.22.240.97:4002`.
- `nautilus-equity-trend` node is **retired/disabled** (2026-06-10; execution
  moved to the game box) — kept in tree but off.
- sing-box hysteria2 inbound (UDP 8443), same rationale as amd-001.
- SSH authorized keys resolved via Kanidm (`authorizedKeysCommand`).
- **s3fs** FUSE mount of an iDrive e2 "docs" bucket at `/mnt/s3/docs`
  (credentials from sops template).
- nginx mixin; ACME; LDAP(S)/IMAPS ports open in the firewall.

---

### `oracle-arm-001` — Oracle Cloud ARM (mostly idle)

- Base server profile: core+server tiers, ACME, nginx mixin, disko disk layout.
- `nodejs_24` as the only notable extra package.
- A fully-specified PostgreSQL block exists but is **disabled** (`enable = false`).
- Wide firewall surface declared (SMB/NetBIOS 137–139/445, 5432, 7000, BGP/Babel…).
- Three additional root SSH authorized keys beyond the standard set.

---

### `oracle-arm-002` — Oracle Cloud ARM (app & data workhorse)

The busiest host. See [`STACK.md`](../nixos-configurations/oracle-arm-002/STACK.md)
for API-level details of the backend stack.

**Supabase-style backend** (`postgres.nix`)
- PostgreSQL 18 JIT with ~30 extensions: TimescaleDB, pgvector + pgvectorscale,
  pg_graphql, pg_cron, pg_net, pgsodium, pgmq, postgis, pg_stat_statements,
  plan_filter, … TLS enforced; logical replication enabled for Realtime.
- **PostgREST** REST API at `api.panda.qzz.io` (Auth0 JWT roles via namespaced
  claim; rate-limited public preview views).
- **supabase-realtime** websocket service at `realtime.panda.qzz.io` with a
  DNS-01 wildcard cert for per-tenant subdomains.
- GoTrue auth is retired (migrated to Auth0); the `auth.*` SQL helpers remain.
- Nightly `postgresqlBackup` of the `api` database.

**LLM platform** (`dify.nix`)
- **Dify** at `dify.panda.qzz.io` — local Postgres (+pgvector) and Redis, local
  storage, nginx vhost with ACME cert.

**Password cracking** (`hashtopolis.nix`)
- **Hashtopolis server** at `hashtopolis.panda.qzz.io` with local MariaDB;
  patched package (upstream lock-path/composer fixes); 1 GB PHP memory limit.

**Quant stack** (`nautilus.nix`, `freqtrade-ohlc.nix`)
- `freqtrade-ohlc-sync` timer: pulls BTC/ETH/BNB/SOL 1-minute OHLC into
  TimescaleDB every 15 min, refreshes 15m/1h/1d views.
- NautilusTrader nodes (all testnet): `nautilus-accumulator` (BTCUSDT smart DCA),
  `nautilus-trend` (Donchian breakout on ETH/BTC/SOL), `nautilus-signal`
  (PUMP/DUMP + accumulation-dip alerts to Telegram), `quant-collectors`
  (news RSS + market stress index feeding the local DB).
- Backup module covers all `/var/lib/nautilus-*` state + DB dumps.
- Datadog agent; nginx mixin; ACME.

---

## Templates / image builders (not deploy targets)

### `iso.nix` → installer ISO

Built with `nix build .#packages.x86_64-linux.iso`.

- Graphical GNOME Calamares installer base.
- Adds btrfs + bcachefs support (ZFS forced off due to kernel conflict).
- Ships gnupg (with full agent/SSH support) and sops for secret decryption during install.
- Flakes enabled, personal cachix substituters pre-trusted, latest kernel.

### `digitalocean.nix` → DigitalOcean image

- Wraps `virtualisation/digital-ocean-image.nix`; GRUB on `/dev/vda`, 2 GB swapfile.

### `generic-nixos/` → generic guest template

- QEMU-guest profile + disko layout targeting `/dev/mmcblk0` (SBC eMMC or virtio disk).
- Just the `root` module, OpenSSH, curl/gitMinimal — a blank slate for
  `nixos-anywhere` style provisioning.

### `digital/` → DigitalOcean droplet snapshot

- Static eth0/eth1 addressing captured from a live droplet, udev interface renaming.
- `do-agent` (DigitalOcean monitoring) enabled.
- sops secrets declared: `wireguard/digital`, `authentik/env` (an Authentik IdP
  was/was intended to run here).
- core+server tiers; kept for reference/rebuilds, not currently deployed from flake.nix.

---

## Shared module glossary (`ezModules.*`)

What hosts are actually pulling in when they import these:

| Module | Provides |
|---|---|
| `root` | root account + hardened sshd |
| `"freeman.xiong"` | primary user account |
| `core` | sane system defaults (locale, nix, networking baseline) |
| `server` | server CLI toolset (python3, eza, mtr, ldns, websocat…), rust-motd banner, backup + security submodules |
| `client-cli` | pcscd, npm, nix-ld (client-side CLI extras) |
| `gui` | desktop application stack |
| `wayland` | niri compositor + XDG portals |
| `greetd` | login manager |
| `misc` | misc system tweaks |
| `dvorak` | Dvorak keyboard layout |
| `tlp` | laptop power management |
| `vr` | VR runtime support |
| `nas` | NAS services |
| `virtualisation` | libvirt/container support |
| `dn42` | DN42 routing baseline |
| `bird-inner` / `bird-border` | BIRD2/Babel mesh routing (inner zone / DN42 border) |
| `acme` | Let's Encrypt DNS-01 via Cloudflare; wildcard `*.panda.qzz.io` cert |
| `sing-box` | proxy client framework (+ `my.sing-box-hysteria2` inbound variant) |
| `mixins-nginx` | shared nginx settings/hardening |
| `datadog-agent` | Datadog monitoring agent |
| `falcon-sensor` | CrowdStrike Falcon sensor |
| `openfortivpn-config` | Fortinet corporate VPN client |

Home-manager CLI tiers stack underneath:
`cli-minimal → cli-server → cli-development` (see README).
