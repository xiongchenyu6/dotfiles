# NixOS System Configurations

A comprehensive NixOS configuration system using [ez-configs](https://github.com/ehllie/ez-configs) for maintainable and modular system management.

## Features

### 🔧 Core System Management
- Latest NixOS packages from `nixpkgs/nixos-unstable`
- Legacy support through `nixpkgs-old` (nixos-21.11)
- Modular configuration using `ez-configs` for clean and maintainable system definitions
- [flake-parts](https://github.com/hercules-ci/flake-parts) based architecture
- Hashicorp Nomad integration with [nomad-driver-nix](https://github.com/input-output-hk/nomad-driver-nix)

### 🖥️ Hardware Support

#### nixos-hardware
Powered by [nixos-hardware](https://github.com/NixOS/nixos-hardware/) for seamless and robust hardware compatibility. It provides device-specific modules that contain kernel parameters, firmware, and driver configurations. You can easily target a specific hardware profile in your configuration, for example:

```nix
{
  imports = [
    # Built-in hardware profiles
    ./hardware-configuration.nix

    # Additional hardware support from nixos-hardware
    (import (fetchTarball "https://github.com/NixOS/nixos-hardware/archive/master.tar.gz") {
      system = "x86_64-linux";
    } + "/lenovo/thinkpad/x1-carbon/6th")
  ];
  ...
}
```

### ⚙️ Extra Tools

#### nix-alien
Using [nix-alien](https://github.com/thiagokokada/nix-alien) to run foreign binaries without polluting your system environment. It automatically detects dependencies of ELF binaries and runs them inside Nix shells. Great for quick usage of proprietary or precompiled binaries.

```bash
# Example usage:
nix-alien ./some-foreign-binary
```

### 📁 Persistent Configuration Management

Using [impermanence](https://github.com/nix-community/impermanence) for declarative state management:

#### Application Configurations
Managed through bind mounts from dotfiles repository:
```plaintext
# Example mount structure
/home/freeman.xiong/dotfiles/stow-managed/
├── albert/
│   └── .local/share/albert → ~/.local/share/albert
├── config/
│   └── .config/
│       ├── Code → ~/.config/Code
│       ├── albert → ~/.config/albert
│       ├── emacs → ~/.config/emacs
│       ├── nvim → ~/.config/nvim
│       └── xmonad → ~/.config/xmonad
├── password-store/
│   └── .local/share/password-store → ~/.local/share/password-store
└── rime/
    └── .local/share/fcitx5/rime → ~/.local/share/fcitx5/rime
```

#### Features:
- Declarative configuration persistence
- Version-controlled application settings
- Easy system replication
- Clean separation of system and user state
- Efficient backup management

#### Persistent Directories:
```nix
# Example persistence configuration
{
  environment.persistence."/persistent" = {
    directories = [
      "/etc/nixos"
      "/etc/ssh"
      "/var/log"
      "/var/lib/postgresql"
      "/var/lib/acme"
    ];
  };
}
```

### 🖥️ Desktop Environments

#### XMonad (X11)
- Highly customizable tiling window manager written in Haskell
- Features:
  - Dynamic workspace management
  - Multi-monitor support with independent workspaces
  - Custom keybindings for efficient workflow
  - Scratchpad terminal and floating windows
  - Window rules for application-specific behavior
  - Status bar integration with xmobar
  - Application launchers (rofi/dmenu)
- Compositor: picom for smooth animations and transparency
- Notification system: dunst

#### Hyprland (Wayland)
- Modern Wayland compositor with advanced features
- Configuration:
  - Adaptive workspace management
  - Dynamic tiling with smart gaps
  - Hardware-accelerated animations
  - Gesture support for touchpads
  - Multi-monitor with variable refresh rates
  - FreeSync/G-SYNC compatibility
- Tools and Integration:
  - waybar for system status
  - wofi for application launching
  - swaylock for screen locking
  - wl-clipboard for clipboard management
  - grim/slurp for screenshots
  - mako for notifications

#### Shared Features
- Display Manager: greetd with custom greeter
- Theme Management:
  - GTK theme configuration
  - Qt/KDE integration
  - Icon themes
  - Cursor themes
- Input Device Configuration:
  - Keyboard layouts (QWERTY/Dvorak)
  - Mouse/touchpad settings
  - Tablet support
- Media Controls:
  - PulseAudio/PipeWire integration
  - Brightness control
  - Volume control
  - Media player controls

### 🌐 Input Method Support

#### Chinese Input with Rime
- Using [rime-frost](https://github.com/gaboolic/rime-frost) for efficient Chinese input through Rime
- Integration with fcitx5 for system-wide input method support
- Configuration managed through stow at `/home/freeman.xiong/dotfiles/stow-managed/rime/.local/share/fcitx5/rime`
  - Symlinked to `~/.local/share/fcitx5/rime` for system use
  - Version controlled and easily synchronized across systems
- Features:
  - Smart candidate ranking with frequently used characters
  - Simplified/Traditional Chinese conversion
  - Custom vocabulary and phrases
  - Cloud sync capability for personal dictionary


### 🔐 Security & Secret Management

#### Secret Management with SOPS
- Using [sops-nix](https://github.com/Mic92/sops-nix) for secure secret encryption and management
- Secrets stored in `/home/freeman.xiong/dotfiles/secrets/`:
  - `common.yaml` - Shared secrets across systems
  - `common.env` - Environment-specific secrets
- Features:
  - Age-based encryption for secure secret storage
  - Version control safe (encrypted secrets can be committed)
  - Automatic secret rotation and management
  - Integration with NixOS configurations

#### Additional Security Features
- SSH hardening and secure configuration
- Kerberos authentication integration
- LDAP and SASL authentication support
- Automated ACME certificate management

### 🛠 Development Environment
- VSCode remote development support via [nixos-vscode-server](https://github.com/nix-community/nixos-vscode-server)
- Custom NUR packages from [xiongchenyu6/nur-packages](https://github.com/xiongchenyu6/nur-packages)
- Complete development toolchain with:
  - Language servers (nixd, gopls, yaml-language-server)
  - Code formatting and linting tools
  - Shell integration (zsh with completions)
- Pre-commit hooks for code quality

### ⚡ Performance & Reliability
- System optimization with [srvos](https://github.com/nix-community/srvos) mixins

### 🌐 DN42 Network Integration

#### DN42 and BGP Configuration
- DN42 module located at `/home/freeman.xiong/dotfiles/nixos-modules/dn42`
  - ROA (Route Origin Authorization) management
  - Registry data synchronization
  - Peering configuration

#### Bird Routing Configuration
- Border routing: `/home/freeman.xiong/dotfiles/nixos-modules/bird-border.nix`
  - BGP peering with DN42 nodes
  - Route filtering and announcement
  - Transit configuration
- Internal routing: `/home/freeman.xiong/dotfiles/nixos-modules/bird-inner.nix`
  - Babel protocol for internal network routing
  - Seamless integration with BGP
  - Automatic neighbor discovery

#### WireGuard Integration
BGP server configuration in `/home/freeman.xiong/dotfiles/nixos-configurations/mail/default.nix`:
- WireGuard interfaces setup:
  - Multiple peer support (office, game, DN42 peers)
  - IPv4 and IPv6 address configuration (fe80::100/64 link-local)
  - Managed through sops-nix (privateKeyFile)
  - Allowed IP ranges:
    - DN42 networks: 172.20.0.0/14
    - Local networks: 10.0.0.0/8, 172.31.0.0/16
    - IPv6 ranges: fd00::/8, fd48:4b4:f3::/48

#### Example Peer Configuration
```nix
wg_theresa = {
  privateKeyFile = config.sops.secrets."wireguard/mail".path;
  address = [ "fe80::100/64" ];
  table = "off";
  listenPort = 23396;
  peers = [{
    endpoint = "cn2.dn42.theresa.cafe:22616";
    publicKey = "MqKkzCwYfOg8Fc/pRRctLW3jS72ACBDQr8ZF10sZ614=";
    allowedIPs = [
      "10.0.0.0/8"
      "172.20.0.0/14"
      "fd00::/8"
    ];
  }];
};
```

#### Babel Protocol Integration
- Automatic redistribution of DN42 routes:
  ```nix
  services.babeld.extraConfig = ''
    redistribute ip 172.20.0.0/14
    redistribute if ens5 deny
  '';
  ```
- Interface-specific configurations for WireGuard tunnels:
  - hello-interval: 5 seconds
  - split-horizon: auto
  - type: wired

- Trusted Nix cache configuration
- Experimental Nix features enabled
- Tracing support for debugging
- Automated upgrades and maintenance

### 🖥️ System Configurations

#### Desktop/Workstation
- Choice of desktop environments (XMonad/Hyprland)
- Gaming optimizations with NVIDIA drivers
- VR support
- Multimedia and productivity tools
- WSL integration for Windows hosts

#### Server
- Mail server with full email stack
- NetBird VPN server
- DNS and ACME certificate management
- Monitoring with Prometheus and Grafana
- Log management
- Web applications support (Discourse, Gitea, Wiki.js, etc.)
- K3s Kubernetes configurations
- Hashicorp Nomad orchestration

#### Special Environments
- NixOS Anywhere for remote deployment
- ISO generation for system installation
- Cloud provider configurations (AWS, Digital Ocean, etc.)

## Getting Started

### Prerequisites
- Nix with flakes enabled
- Git
- SSH access for remote deployment

### Installation

1. Clone the repository:
```bash
git clone https://github.com/yourusername/dotfiles.git
cd dotfiles
```

2. Enter the development shell:
```bash
nix develop
```

3. Deploy a configuration:
```bash
# For local deployment
nixos-rebuild switch --flake .#<configuration>

# For remote deployment
nixos-rebuild switch --flake .#heco-zammad --build-host root@game --target-host root@10.16.0.96 --impure --fast
```

### Environment Setup

The development shell automatically configures:
```bash
export NIX_SSHOPTS="-Y -p 22"
export PULUMI_CONFIG_PASSPHRASE=""
```

## Development Tools

The development shell includes:

### System Management
- `nixos-rebuild-ng` for system rebuilding
- `nixos-facter` for system information
- `nixos-anywhere` for remote deployment
- `nixfmt-rfc-style` for Nix formatting
- `statix` for static analysis

### Secret Management
- `sops` for secret management
- `ssh-to-age` for key conversion

### Documentation & Configuration
- `mdbook` for documentation
- `dasel` and `yq-go` for YAML/TOML processing
- `editorconfig-checker` for consistent styling

### Development
- Language servers:
  - `nixd` for Nix
  - `gopls` for Go
  - `yaml-language-server` for YAML
- Shell integration:
  - `zsh` with enhanced features
  - `zsh-completions` for better CLI experience

## Quality Assurance

### Pre-commit Hooks

Automatically enforced code quality checks:
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

### Continuous Integration

- Automated checks on pull requests
- System build verification
- Configuration validation

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Acknowledgments

- [srvos](https://github.com/nix-community/srvos) for system optimizations
- [ez-configs](https://github.com/ehllie/ez-configs) for configuration management
- [flake-parts](https://github.com/hercules-ci/flake-parts) for modular flake structure
- [impermanence](https://github.com/nix-community/impermanence) for state management
- [nixos-hardware](https://github.com/NixOS/nixos-hardware/) for robust hardware support
- [nix-alien](https://github.com/thiagokokada/nix-alien) for running foreign binaries
