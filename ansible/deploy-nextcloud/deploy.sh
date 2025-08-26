#!/bin/bash
set -e

echo "Nextcloud Ansible Deployment Script"
echo "==================================="
echo ""

# Check if ansible is installed
if ! command -v ansible &> /dev/null; then
    echo "Error: Ansible is not installed"
    echo "Please install ansible: pip install ansible"
    exit 1
fi

# Check connectivity to target host
echo "Checking connectivity to 10.171.150.101..."
if ! ansible -i inventory.ini nextcloud -m ping &> /dev/null; then
    echo "Error: Cannot connect to target host"
    echo "Please ensure:"
    echo "  1. SSH access to root@10.171.150.101 is configured"
    echo "  2. The server is reachable"
    exit 1
fi

echo "✓ Connection successful"
echo ""

# Prompt for confirmation
echo "This will install Nextcloud on 10.171.150.101"
echo "Configuration:"
echo "  - Nextcloud version: 29.0.4"
echo "  - Data directory: /var/nextcloud-data"
echo "  - Database: MariaDB"
echo "  - Web server: Apache"
echo ""
read -p "Do you want to continue? (y/N): " -n 1 -r
echo ""

if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Deployment cancelled"
    exit 0
fi

echo ""
echo "Starting deployment..."
echo ""

# Run the playbook
ansible-playbook -i inventory.ini playbook.yml

echo ""
echo "Deployment complete!"
echo ""
echo "Access Nextcloud at: http://10.171.150.101"
echo "Admin username: admin"
echo "Admin password: (see vars/main.yml)"
echo ""
echo "⚠️  IMPORTANT: Change all default passwords before production use!"