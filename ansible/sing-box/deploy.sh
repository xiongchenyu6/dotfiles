#!/usr/bin/env bash

# Secure deployment script for sing-box using SOPS
set -e

echo "========================================="
echo "Sing-box Secure Deployment"
echo "========================================="

# Check prerequisites
if ! command -v ansible-playbook &> /dev/null; then
    echo "Error: ansible-playbook is not installed"
    echo "Please install Ansible first"
    exit 1
fi

if ! command -v sops &> /dev/null; then
    echo "Error: sops is not installed"
    echo "Please install SOPS first"
    exit 1
fi

# Check if secrets file exists and can be decrypted
if [ ! -f "secrets/servers.yaml" ]; then
    echo "Error: secrets/servers.yaml not found"
    exit 1
fi

echo "Verifying SOPS encryption..."
if ! sops -d secrets/servers.yaml > /dev/null 2>&1; then
    echo "Error: Cannot decrypt secrets/servers.yaml"
    echo "Please ensure you have the correct GPG key"
    exit 1
fi

echo "✓ SOPS encryption verified"
echo ""

# Run the secure deployment
echo "Running secure deployment..."
ansible-playbook -i inventory.ini deploy-singbox-secure.yml "$@"

echo ""
echo "========================================="
echo "Deployment complete!"
echo ""
echo "To view credentials:"
echo "  sops -d secrets/servers.yaml"
echo "========================================="