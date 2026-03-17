# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Is

Nix Flake monorepo managing NixOS hosts, nix-darwin (macOS), and home-manager configurations across cloud servers (Oracle, Tencent, Huoshan) and local machines (desktop, office, gaming). Uses `ez-configs` flake-parts module for convention-based host/module discovery.

## Common Commands

```bash
# Enter dev shell (provides sops, nixfmt, statix, nixos-anywhere, etc.)
direnv allow   # or: nix develop

# Build/deploy NixOS host
nixos-rebuild switch --flake .#<hostname>
nixos-rebuild switch --flake .#<hostname> --target-host root@<ip>

# Build/deploy macOS
darwin-rebuild switch --flake .#office-mac

# Build ISO
nix build .#packages.x86_64-linux.iso

# Manage secrets
sops secrets/common.yaml

# Check flake outputs
nix flake show
nix flake check
```

Pre-commit hooks run: statix, deadnix, shellcheck, shfmt (nixfmt is disabled).

## Architecture

### ez-configs Convention

The flake uses `ez-configs` which auto-discovers modules by directory convention:
- `nixos-configurations/<hostname>/` → NixOS system config for that host
- `nixos-modules/` → shared NixOS modules (auto-imported)
- `darwin-configurations/` → macOS system configs
- `darwin-modules/` → shared Darwin modules
- `home-configurations/` → per-user home-manager configs
- `home-modules/` → shared home-manager modules
- `shared-modules/` → modules applied to both NixOS and Darwin

Global args passed to all modules: `inputs`, `shares`, `mylib`.

### Key Files

- **`flake.nix`** — entry point; defines all inputs, hosts, and the ez-configs mapping
- **`shares.nix` / `shares.toml`** — shared data (user info, SSH keys, network config)
- **`lib/default.nix`** — custom Nix library (Bird2/DN42 routing config factory)
- **`.sops.yaml`** — SOPS encryption rules (age keys per host + PGP)

### Home-Manager CLI Tiers

Modules are layered — each tier imports the previous:
```
cli-minimal       → base CLI tools (all users including root)
  └─ cli-server   → adds dev/ops tools for servers
       └─ cli-development → full IDE-like environment
```

### Hosts

| Host | Platform | Location |
|------|----------|----------|
| oracle-amd-001/002 | x86_64-linux | Oracle Cloud |
| oracle-arm-001/002 | aarch64-linux | Oracle Cloud |
| tcloud | x86_64-linux | Tencent Cloud |
| huoshan-bj-001 | x86_64-linux | Huoshan (Beijing) |
| game | x86_64-linux | Local (NVIDIA GPU passthrough) |
| office, office-office | x86_64-linux | Local workstations |
| netbird | x86_64-linux | NetBird VPN |
| office-mac | aarch64-darwin | macOS (nix-darwin) |

### Secrets

Uses `sops-nix` with age encryption. Each host has its own age key defined in `.sops.yaml`. Secrets live in `secrets/` directory. Use `sops` CLI to edit encrypted files.

### Peripheral Systems

- **`ansible/`** — deployment playbooks for sing-box, Nextcloud, Wazuh, VersityGW
- **`scripts/`** — GPU passthrough management, NixOS bootstrap, nixos-infect
- **`stow-managed/`** — GNU Stow dotfiles (iTerm, IntelliJ, Rime, etc.)

## Conventions

- Commit messages use conventional prefixes: `feat:`, `fix:`, `chore:`
- Philosophy: incremental progress, composition over inheritance, clear intent over clever code, simplicity
- Platform-specific packages use `lib.optionals` with `stdenv.isDarwin` / `stdenv.isLinux`
