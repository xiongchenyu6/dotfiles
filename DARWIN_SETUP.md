# Darwin (macOS) Setup for M4 Mac

## Prerequisites

1. Install Nix on your Mac if not already installed:
```bash
sh <(curl -L https://nixos.org/nix/install)
```

2. Ensure you have the office-mac age key configured. The key should be at:
   - Age key: `age1n8dtlw45h5gh4zp0l53tcasffzgqflyx623ppypasrukyt8kv3vqljflr3` 
   - Store it at: `~/.config/sops/age/keys.txt` on your Mac

## Deployment Steps

1. Clone this repository:
```bash
git clone <your-repo-url> ~/dotfiles
cd ~/dotfiles
```

2. Enter the development shell (will include darwin-rebuild):
```bash
nix develop
```

3. Build and switch to your Darwin configuration:
```bash
darwin-rebuild switch --flake .#office-mac
```

## WireGuard VPN Setup

The WireGuard configuration has been migrated from the NixOS office machine to work on Darwin. It will:

- Connect to the tcloud server at `43.156.66.157:22616`
- Use the same private key from sops secrets
- Configure IPv4 and IPv6 addresses
- Set up routing for internal networks

### Managing WireGuard

The WireGuard interface is managed by launchd and will start automatically on boot.

To manually stop WireGuard:
```bash
sudo launchctl stop system/org.nixos.wireguard-wg_tcloud
```

To manually start WireGuard:
```bash
sudo launchctl start system/org.nixos.wireguard-wg_tcloud
```

To check WireGuard status:
```bash
sudo wg show
```

### Troubleshooting

1. Check logs:
```bash
tail -f /var/log/wireguard-wg_tcloud.log
tail -f /var/log/wireguard-wg_tcloud.error.log
```

2. Verify age key is properly configured:
```bash
sops -d ~/dotfiles/secrets/common.yaml | grep wireguard
```

3. Ensure the WireGuard tools are available:
```bash
which wg-quick
```

## Architecture Note

The configuration is set for `aarch64-darwin` (Apple Silicon). If you're using an Intel Mac, you'll need to change this in `darwin-modules/default.nix`:
```nix
nixpkgs.hostPlatform = "x86_64-darwin";  # For Intel Macs
```