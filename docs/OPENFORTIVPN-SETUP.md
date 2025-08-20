# OpenFortiVPN Secure Setup for NixOS

## Overview
This setup provides a secure OpenFortiVPN configuration using SOPS for credential encryption, with manual TOTP/OTP input support.

## Configuration Added

### 1. Encrypted Credentials
The following credentials have been added to `secrets/common.yaml` (encrypted with SOPS):
- `openfortivpn/server`: 13.228.50.8
- `openfortivpn/username`: freeman.xiong  
- `openfortivpn/password`: (encrypted)
- `openfortivpn/trusted_cert`: 6a4d98c4d9eb9d1326dad7819f1c32785a243bc0dcd369dac3cc7744563225a6

### 2. NixOS Module
Created `nixos-modules/openfortivpn-config.nix` which:
- Installs the `openfortivpn` package
- Sets up SOPS secrets for credentials
- Generates `/etc/openfortivpn/config` file automatically
- Provides convenience scripts for VPN management

## Usage

### Add to Your NixOS Configuration

Add the module to your system configuration (e.g., in `nixos-configurations/office/default.nix`):

```nix
{
  imports = [
    # ... other imports
    ezModules.openfortivpn-config
  ];
}
```

### Rebuild Your System

```bash
sudo nixos-rebuild switch --flake .#office
```

### Connect to VPN

After rebuilding, use the provided convenience scripts:

#### Interactive Connection (Foreground)
```bash
vpn-connect
# Enter OTP code when prompted
```

#### Background Connection
```bash
vpn-connect-background
# Enter OTP code when prompted
```

#### Check VPN Status
```bash
vpn-status
```

#### Disconnect VPN
```bash
vpn-disconnect
```

#### View VPN Logs
```bash
vpn-logs
```

## Manual Command (Alternative)

If you prefer to use openfortivpn directly:

```bash
# With config file (still need to provide OTP)
sudo openfortivpn -c /etc/openfortivpn/config -o <OTP_CODE>

# Or specify OTP interactively
sudo openfortivpn -c /etc/openfortivpn/config
# Then enter OTP when prompted
```

## Security Features

1. **Encrypted Storage**: All credentials stored in SOPS-encrypted `secrets/common.yaml`
2. **Protected Config**: Config file at `/etc/openfortivpn/config` has 600 permissions (root only)
3. **No Plain Text**: Password never appears in plain text in the repository
4. **Manual TOTP**: OTP/TOTP codes must be entered manually for each connection (2FA security)

## Troubleshooting

### Config File Not Found
If you see "Config file not found", ensure:
1. You've added the module to your NixOS configuration
2. You've rebuilt the system with `nixos-rebuild switch`
3. SOPS secrets are properly decrypted

### Connection Issues
1. Check VPN logs: `vpn-logs`
2. Verify credentials are correct in encrypted secrets
3. Ensure TOTP/OTP code is current (they expire quickly)
4. Check network connectivity to the VPN server

### View Decrypted Credentials (for debugging)
```bash
# View all OpenFortiVPN secrets
sops -d secrets/common.yaml | grep -A5 "openfortivpn:"

# Check if config was generated
sudo cat /etc/openfortivpn/config
```

## File Locations

- **Encrypted Secrets**: `/home/freeman.xiong/dotfiles/secrets/common.yaml`
- **NixOS Module**: `/home/freeman.xiong/dotfiles/nixos-modules/openfortivpn-config.nix`
- **Generated Config**: `/etc/openfortivpn/config` (created on system activation)
- **This Documentation**: `/home/freeman.xiong/dotfiles/docs/OPENFORTIVPN-SETUP.md`