#!/usr/bin/env bash

echo "=== Hyprland Display Status ==="
echo ""

# Check if Hyprland is running (process name is case-sensitive)
if ! pgrep -x Hyprland > /dev/null && ! pgrep -f "/Hyprland" > /dev/null; then
    echo "Hyprland is not running"
    exit 1
fi

# Check Wayland displays
echo "=== Wayland Outputs ==="
if command -v wlr-randr &> /dev/null; then
    wlr-randr
else
    echo "wlr-randr not found, trying hyprctl..."
fi

echo ""
echo "=== Hyprland Monitors ==="
hyprctl monitors

echo ""
echo "=== DRM Devices ==="
ls -la /dev/dri/

echo ""
echo "=== GPU Cards ==="
for card in /sys/class/drm/card*; do
    if [ -d "$card" ]; then
        echo "$(basename $card):"
        if [ -f "$card/device/vendor" ] && [ -f "$card/device/device" ]; then
            vendor=$(cat "$card/device/vendor")
            device=$(cat "$card/device/device")
            echo "  Vendor: $vendor, Device: $device"
        fi
        # Check if it's NVIDIA
        if [ -L "$card/device/driver" ]; then
            driver=$(readlink "$card/device/driver" | awk -F '/' '{print $NF}')
            echo "  Driver: $driver"
        fi
    fi
done

echo ""
echo "=== Environment Variables ==="
echo "WLR_DRM_DEVICES: $WLR_DRM_DEVICES"
echo "__GLX_VENDOR_LIBRARY_NAME: $__GLX_VENDOR_LIBRARY_NAME"
echo "GBM_BACKEND: $GBM_BACKEND"
echo "WLR_NO_HARDWARE_CURSORS: $WLR_NO_HARDWARE_CURSORS"

echo ""
echo "=== NVIDIA Status ==="
nvidia-smi -L 2>/dev/null || echo "nvidia-smi failed"