#!/usr/bin/env bash

# Secure deployment script for sing-box using SOPS.
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

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

if ! sops --decrypt --extract '["lubancat"]["become_password"]' \
    ../../secrets/common.yaml > /dev/null 2>&1; then
    echo "Error: Cannot decrypt lubancat sudo credential from ../../secrets/common.yaml"
    exit 1
fi

# NixOS does not put Python in PATH on sg-office. Reuse an interpreter already
# present in the Nix store; this is read-only and does not alter its profile.
SING_BOX_SG_OFFICE_PYTHON="$({
    ssh -o BatchMode=yes -o ConnectTimeout=8 root@sg-office \
        'command -v python3 || find /nix/store -maxdepth 3 -type f -path "*/bin/python3" -perm -0100 2>/dev/null | head -n 1'
} 2>/dev/null || true)"

case "$SING_BOX_SG_OFFICE_PYTHON" in
    /*/bin/python3) export SING_BOX_SG_OFFICE_PYTHON ;;
    *) unset SING_BOX_SG_OFFICE_PYTHON ;;
esac

echo "✓ SOPS encryption verified"
echo ""

# Run the secure deployment
echo "Running secure deployment..."
ansible-playbook -i inventory.ini deploy-singbox-secure.yml "$@"

echo ""
echo "========================================="
echo "Deployment complete!"
echo "Routing was intentionally left unchanged. The only firewall change is the"
echo "Shadowsocks port itself on the ufw host; sg-office declares it in NixOS."
echo "========================================="
