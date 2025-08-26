# Nextcloud Ansible Deployment

This Ansible playbook automates the deployment of Nextcloud on Ubuntu/Debian servers.

## Prerequisites

- Target server running Ubuntu 20.04/22.04 or Debian 11/12
- SSH access to the target server as root
- Ansible installed on your local machine

## Installation

1. Install Ansible if not already installed:
```bash
pip install ansible
# or
apt-get install ansible
```

## Configuration

1. Edit `vars/main.yml` to customize:
   - Nextcloud version
   - Domain name
   - Database passwords
   - Admin credentials
   - Data directory location

2. Update `inventory.ini` if your target server IP is different

## Usage

### Deploy Nextcloud

Run the playbook to deploy Nextcloud:

```bash
cd /home/freeman.xiong/dotfiles/ansible/deploy-nextcloud
ansible-playbook playbook.yml
```

### Verify Installation

After deployment, access Nextcloud at:
- http://10.171.150.101 (or your configured domain)
- Login with the admin credentials from `vars/main.yml`

## Security Notes

**IMPORTANT**: Before production use:

1. **Change all default passwords** in `vars/main.yml`:
   - mysql_root_password
   - nextcloud_db_password
   - nextcloud_admin_password

2. **Configure SSL/TLS**:
   - Set up Let's Encrypt or your own SSL certificate
   - Update the Apache configuration for HTTPS

3. **Use Ansible Vault** for sensitive data:
   ```bash
   ansible-vault encrypt vars/main.yml
   ansible-playbook playbook.yml --ask-vault-pass
   ```

## Features Installed

- Apache web server with required modules
- MariaDB database server
- PHP 8.2 with all required extensions
- Redis for caching
- APCu for local caching
- Automatic background jobs via cron
- Security headers configured

## Customization

### Installing Additional Apps

Add apps to the `nextcloud_apps` list in `vars/main.yml` and re-run the playbook.

### Changing PHP Settings

Modify the PHP configuration variables in `vars/main.yml`:
- php_memory_limit
- php_upload_max_filesize
- php_post_max_size

## Troubleshooting

1. **Connection Issues**: Ensure SSH access to 10.171.150.101 as root
2. **Permission Issues**: Check that www-data owns Nextcloud files
3. **Database Issues**: Verify MariaDB is running and credentials are correct
4. **Apache Issues**: Check Apache error logs at `/var/log/apache2/`

## Maintenance

Regular maintenance tasks:
- Keep Nextcloud updated
- Monitor disk space in data directory
- Review logs regularly
- Backup database and data directory