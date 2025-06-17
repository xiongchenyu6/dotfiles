# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This repository contains a comprehensive NixOS configuration system using [ez-configs](https://github.com/ehllie/ez-configs) for maintainable and modular system management. It manages multiple NixOS and darwin systems with shared configurations.

## Development Environment

### Setting Up Development Environment

```bash
# Enter the development shell with all required tools
nix develop

# The development shell automatically configures:
export NIX_SSHOPTS="-Y -p 22"
export PULUMI_CONFIG_PASSPHRASE=""
```

### Common Commands

```bash
# Apply configuration to local system
nixos-rebuild switch --flake .#<configuration>

# Apply configuration to remote system
nixos-rebuild switch --flake .#<target-host> --build-host <build-host> --target-host <target-ip> --impure --fast

# Check for NixOS configuration errors
nixos-rebuild build --flake .#<configuration>

# Format Nix code
nixfmt-rfc-style <file-path>

# Analyze Nix code for static issues
statix check

# Decrypt secrets file
sops -d ./secrets/common.env
```

## Architecture

### System Structure

- **flake.nix**: Main entry point defining inputs, outputs and configurations
- **nixos-configurations/**: Host-specific NixOS configurations
- **nixos-modules/**: Shared NixOS modules
- **home-configurations/**: User-specific home configurations
- **home-modules/**: Shared home-manager modules
- **darwin-configurations/**: MacOS configurations
- **darwin-modules/**: Shared Darwin modules
- **lib/**: Custom Nix functions and utilities
- **secrets/**: Encrypted system secrets
- **stow-managed/**: Configuration files managed via symlinks

### Key Components

1. **ez-configs**: Used for modular configuration using the `ezConfigs` attribute in flake.nix
2. **home-manager**: Manages user environments
3. **flake-parts**: Organizes the flake into modular components
4. **sops-nix**: Handles encrypted secrets
5. **impermanence**: Manages persistent configurations
6. **disko**: Manages disk configurations

### Configuration Flow

1. The flake.nix defines host configurations via the `ezConfigs` attribute
2. Each host imports modules from `nixos-modules/` or `home-modules/`
3. User configurations are defined in `home-configurations/`
4. Specialized configurations like desktop environments are in their dedicated modules

## Desktop Environment Configurations

The repository includes configurations for multiple desktop environments:

- **XMonad (X11)**: Configuration in `home-modules/xmonad/`
- **Hyprland (Wayland)**: Configuration in `home-modules/hyprland/`
- **Keyboard Layouts**:
  - QWERTY: `home-modules/qwert.nix`
  - Dvorak: `home-modules/dvorak.nix`
  - Dvorak Programmer: `home-modules/dvorak-programmer.nix`

## Secret Management

Secrets are managed with sops-nix:
- **Secrets location**: `/home/freeman.xiong/dotfiles/secrets/`
  - `common.yaml` - Shared secrets across systems
  - `common.env` - Environment-specific secrets

## Quality Control

Code quality is maintained through pre-commit hooks defined in flake.nix:

```nix
pre-commit = {
  check.enable = true;
  settings.hooks = {
    nixfmt.enable = true;
    statix.enable = true;
    deadnix.enable = true;
    shellcheck.enable = true;
    shfmt.enable = true;
  };
};
```

When making changes:
1. Make changes to relevant modules
2. Run the pre-commit checks
3. Test the configuration with `nixos-rebuild build`
4. Apply with `nixos-rebuild switch`