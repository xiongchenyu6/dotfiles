# Sing-box Shadowsocks Server Documentation

## Deployment Status
✅ **Successfully deployed** on 2025-08-18

## Server Details
- **Server IP:** 45.194.18.75
- **Port:** 8388
- **Protocol:** Shadowsocks
- **Encryption Method:** 2022-blake3-aes-128-gcm
- **Service Status:** Active and running

## 🔐 Accessing Credentials

Credentials are securely stored using SOPS encryption. To view them:

```bash
# View all credentials
sops -d secrets/servers.yaml

# Extract just the Shadowsocks password
sops -d secrets/servers.yaml | grep -A1 "shadowsocks:" | grep "password:" | awk '{print $2}' | tr -d '"'
```

## Client Configuration

To generate client configuration:
```bash
# Get the password
PASSWORD=$(sops -d secrets/servers.yaml | grep -A3 "shadowsocks:" | grep "password:" | awk '{print $2}' | tr -d '"')

# Create client config
cat << EOF
{
  "server": "45.194.18.75",
  "server_port": 8388,
  "method": "2022-blake3-aes-128-gcm",
  "password": "$PASSWORD"
}
EOF
```

## Service Management

### Check service status
```bash
ssh root@45.194.18.75 "systemctl status sing-box"
```

### Restart service
```bash
ssh root@45.194.18.75 "systemctl restart sing-box"
```

### View logs
```bash
ssh root@45.194.18.75 "journalctl -u sing-box -f"
```

## Firewall Configuration
- Port 8388/tcp - Open for Shadowsocks
- Port 22/tcp - Open for SSH management
- UFW firewall is enabled and configured

## Installation Details
- **Sing-box Version:** 1.10.7
- **Installation Path:** `/usr/local/bin/sing-box`
- **Config Location:** `/etc/sing-box/config.json`
- **System User:** sing-box (unprivileged)
- **Working Directory:** `/var/lib/sing-box`

## Security Features
- Running as unprivileged system user
- Systemd service with automatic restart on failure
- Modern encryption (2022-blake3-aes-128-gcm)
- Multiplexing enabled for better performance
- Ad-blocking rules included (blocks category-ads-all)

## Troubleshooting

If connection issues occur:
1. Verify service is running: `ssh root@45.194.18.75 "systemctl status sing-box"`
2. Check firewall: `ssh root@45.194.18.75 "ufw status"`
3. Review logs: `ssh root@45.194.18.75 "journalctl -u sing-box -n 50"`

## Files Created During Deployment
- `/home/freeman.xiong/dotfiles/ansible-sing-box/inventory.ini` - Ansible inventory
- `/home/freeman.xiong/dotfiles/ansible-sing-box/deploy-singbox.yml` - Ansible playbook
- `/home/freeman.xiong/dotfiles/ansible-sing-box/files/config.json` - Config template
- `/home/freeman.xiong/dotfiles/ansible-sing-box/quick-deploy.sh` - Quick deployment script
- `/home/freeman.xiong/dotfiles/ansible-sing-box/credentials.txt` - Basic credentials file
- `/home/freeman.xiong/dotfiles/ansible-sing-box/host_vars/ubuntu-server.yml` - Server SSH credentials