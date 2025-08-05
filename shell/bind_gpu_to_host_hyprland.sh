#!/usr/bin/env bash

# GPU PCI addresses
GPU="0000:01:00.0"
AUDIO="0000:01:00.1"
DRIVER="nvidia"

# Check if running as root
if [ "$EUID" -ne 0 ]; then 
    echo "Please run as root (use sudo)"
    exit 1
fi

echo "=== Unbinding GPU from vfio-pci driver ==="
if [ -L /sys/bus/pci/devices/$GPU/driver ]; then
    echo "$GPU" > /sys/bus/pci/devices/$GPU/driver/unbind
    echo "GPU unbound from vfio-pci"
else
    echo "GPU not bound to any driver"
fi

if [ -L /sys/bus/pci/devices/$AUDIO/driver ]; then
    echo "$AUDIO" > /sys/bus/pci/devices/$AUDIO/driver/unbind
    echo "Audio unbound from vfio-pci"
else
    echo "Audio not bound to any driver"
fi

# Remove vfio-pci if loaded
echo "=== Removing vfio-pci module if loaded ==="
modprobe -r vfio-pci 2>/dev/null || echo "vfio-pci not loaded"
modprobe -r vfio_iommu_type1 2>/dev/null
modprobe -r vfio 2>/dev/null

# Reset the PCI devices
echo "=== Resetting PCI devices ==="
echo 1 > /sys/bus/pci/devices/$GPU/remove
echo 1 > /sys/bus/pci/devices/$AUDIO/remove
sleep 1
echo 1 > /sys/bus/pci/rescan
sleep 2

# Load NVIDIA modules with Wayland support
echo "=== Loading NVIDIA kernel modules ==="
modprobe nvidia
modprobe nvidia_modeset
modprobe nvidia_uvm
# Enable DRM for Wayland
modprobe nvidia_drm modeset=1

# Bind devices to their drivers
echo "=== Binding GPU to nvidia driver ==="
echo "$GPU" > /sys/bus/pci/drivers/$DRIVER/bind || echo "Failed to bind GPU"
echo "$AUDIO" > /sys/bus/pci/drivers/snd_hda_intel/bind || echo "Failed to bind audio"

# Wait for driver to settle
sleep 2

# Set NVIDIA DRM modeset
echo "=== Enabling NVIDIA DRM modeset ==="
echo "options nvidia-drm modeset=1" > /etc/modprobe.d/nvidia-drm-nomodeset.conf

# For Hyprland, we need to reload the compositor
echo "=== Hyprland-specific steps ==="
echo "You need to:"
echo "1. Exit Hyprland (Super+M or your logout keybind)"
echo "2. Log back in"
echo ""
echo "Or run this command from another TTY:"
echo "  systemctl restart greetd  # or your display manager"
echo ""
echo "Make sure your hyprland.conf has:"
echo "  env = WLR_DRM_DEVICES,/dev/dri/card1:/dev/dri/card0"
echo "  env = __GLX_VENDOR_LIBRARY_NAME,nvidia"
echo "  env = GBM_BACKEND,nvidia-drm"