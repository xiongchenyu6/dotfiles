#!/usr/bin/env bash
set -euo pipefail

# Loads nvidia_drm with modeset=1 to fix Hyprland DRM issues.

usage() {
    echo "Usage: $0"
    echo "Fixes the NVIDIA DRM for Hyprland by loading the nvidia_drm module with modeset=1."
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
    usage
    exit 0
fi

echo "=== Fixing NVIDIA DRM for Hyprland ==="

# Check if running as root
if [ "$EUID" -ne 0 ]; then 
    echo "Please run as root (use sudo)"
    exit 1
fi

# Load nvidia_drm with modeset
echo "Loading nvidia_drm with modeset=1..."
modprobe nvidia_drm modeset=1

# Check if successful
if lsmod | grep -q nvidia_drm; then
    echo "nvidia_drm loaded successfully"
else
    echo "Failed to load nvidia_drm"
    exit 1
fi

# Check for NVIDIA card in DRM
echo ""
echo "Checking DRM devices..."
ls -la /dev/dri/

# Check if card0 exists (should be NVIDIA)
if [ -e /dev/dri/card0 ]; then
    echo ""
    echo "NVIDIA DRM device found at /dev/dri/card0"
    echo "You may need to restart Hyprland to detect external displays"
else
    echo ""
    echo "Warning: NVIDIA DRM device not found"
    echo "Try: systemctl restart systemd-logind"
fi

echo ""
echo "To make this permanent, ensure your NixOS config has:"
echo "  boot.kernelParams = [ \"nvidia-drm.modeset=1\" ];"
echo "  hardware.nvidia.modesetting.enable = true;"
