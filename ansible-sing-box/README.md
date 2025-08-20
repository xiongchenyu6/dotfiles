# Ansible Sing-box Deployment with SOPS Encryption

This repository contains Ansible playbooks for deploying sing-box with Shadowsocks proxy on Ubuntu servers, using SOPS for secure credential management.

## 🔐 Security Features

- **SOPS Encryption**: All sensitive data (passwords, keys) are encrypted using SOPS with GPG
- **GitIgnore**: Properly configured to prevent accidental commits of sensitive data
- **Secure Playbooks**: Credentials are never stored in plain text in the repository

## 📋 Prerequisites

1. **Ansible** installed locally
2. **SOPS** installed locally (`nix-env -iA nixpkgs.sops` or `brew install sops`)
3. **GPG key** for encryption (already configured)
4. **community.sops** Ansible collection:
   ```bash
   ansible-galaxy collection install community.sops
   ```

## 🗂️ Repository Structure

```
ansible-sing-box/
├── .sops.yaml              # SOPS configuration
├── ansible.cfg             # Ansible configuration with SOPS plugin
├── inventory.ini           # Ansible inventory
├── secrets/
│   └── servers.yaml        # Encrypted credentials (SOPS)
├── deploy-singbox-secure.yml # Secure deployment playbook
├── files/
│   └── config.json         # Sing-box config template
└── .gitignore             # Excludes sensitive files
```

## 🔑 Managing Secrets with SOPS

### View Decrypted Secrets
```bash
sops -d secrets/servers.yaml
```

### Edit Encrypted Secrets
```bash
sops secrets/servers.yaml
```

### Encrypt a New File
```bash
sops -e -i newfile.yaml
```

### Rotate Encryption Keys
```bash
sops -r secrets/servers.yaml
```

## 🚀 Deployment

### Run the Secure Deployment
```bash
ansible-playbook -i inventory.ini deploy-singbox-secure.yml
```

The playbook will:
1. Automatically decrypt credentials using SOPS
2. Deploy sing-box with Shadowsocks
3. Configure firewall rules
4. Start the service

## 📝 Encrypted Credentials Structure

The `secrets/servers.yaml` file contains:
```yaml
servers:
  ubuntu_server:
    host: <server-ip>
    ssh_user: root
    ssh_password: <encrypted>
    
shadowsocks:
  password: <encrypted>
  port: 8388
  method: "2022-blake3-aes-128-gcm"
```

## 🔄 Adding New Servers

1. Edit the encrypted file:
   ```bash
   sops secrets/servers.yaml
   ```

2. Add new server configuration:
   ```yaml
   servers:
     new_server:
       host: x.x.x.x
       ssh_user: root
       ssh_password: "password"
   ```

3. Update `inventory.ini`:
   ```ini
   [singbox_servers]
   new-server ansible_host=x.x.x.x ansible_user=root
   ```

## 🛡️ Security Best Practices

1. **Never commit plain text credentials**
2. **Always use SOPS for sensitive data**
3. **Keep your GPG key secure**
4. **Regularly rotate passwords**
5. **Use `.gitignore` to exclude sensitive files**

## 🔧 Troubleshooting

### SOPS Decryption Fails
```bash
# Check GPG key is available
gpg --list-secret-keys

# Check SOPS configuration
cat .sops.yaml
```

### Ansible Can't Find SOPS Plugin
```bash
# Install the collection
ansible-galaxy collection install community.sops

# Verify installation
ansible-galaxy collection list | grep sops
```

## 📊 Service Management

After deployment, manage the service on the server:

```bash
# Check status
ssh root@<server-ip> "systemctl status sing-box"

# View logs
ssh root@<server-ip> "journalctl -u sing-box -f"

# Restart service
ssh root@<server-ip> "systemctl restart sing-box"
```

## 🗑️ Cleanup

To remove sensitive files before committing:
```bash
# Remove any decrypted files
rm -f secrets/*.dec.yaml

# Remove temporary credentials
rm -f credentials.txt host_vars/*.yml

# Verify with git status
git status
```

## ⚠️ Important Notes

- The GPG key fingerprint used: `3D7331009E93CC97A8CA809D03DFD2DEA7AF6693`
- SOPS version: 3.10.2
- Sing-box version: 1.10.7
- This setup is configured for your GPG key. Other users will need access to the same GPG key or you'll need to add their keys to `.sops.yaml`

## 📚 References

- [SOPS Documentation](https://github.com/mozilla/sops)
- [Ansible SOPS Collection](https://docs.ansible.com/ansible/latest/collections/community/sops/index.html)
- [Sing-box Documentation](https://sing-box.sagernet.org/)