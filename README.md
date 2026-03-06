# Dotfiles

Nix flake managing my laptop, NUR, and cloud hosts — covering NixOS, Darwin, and home-manager configurations.

## Philosophy

- **Incremental progress over big bangs** — small changes that compile and pass checks
- **Learning from existing code** — study and plan before implementing
- **Pragmatic over dogmatic** — adapt to project reality
- **Clear intent over clever code** — be boring and obvious
- **Composition over inheritance** — explicit data flow and dependencies
- **Simplicity** — single responsibility, no premature abstractions, choose the boring solution

## Module Hierarchy

### CLI Tiers

```
cli-minimal        Base CLI tools for all users (root + non-root)
  └─ cli-server    Server tier — extends with dev/ops tools
       └─ cli-development  Full development tier — extends with IDE-like tools
```

### GUI Modules

```
gui/
  default.nix      Entry point — imports packages.nix + linux.nix
  packages.nix     Cross-platform GUI apps and CLI tools
  linux.nix        Linux-only desktop config (GTK, Qt, i18n, etc.)
```

### Integration Modules

- `nixos-integration.nix` — NixOS desktop integration (XDG, services, dconf)
- `hyprland/` — Hyprland window manager configuration

## Structure

```
├── flake.nix                 Flake entry point
├── nixos-configurations/     Per-host NixOS system configs
├── nixos-modules/            Shared NixOS modules
├── darwin-configurations/    macOS system configs
├── darwin-modules/           Shared Darwin modules
├── home-configurations/      Per-user home-manager configs
├── home-modules/             Shared home-manager modules
├── shared-modules/           Modules shared across NixOS and Darwin
├── secrets/                  sops-encrypted secrets
├── scripts/                  Utility scripts
└── stow-managed/             GNU Stow-managed dotfiles
```
