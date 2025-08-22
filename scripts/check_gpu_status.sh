#!/usr/bin/env bash
set -euo pipefail

# This script checks the binding status of the GPU and Audio devices, and the status of the NVIDIA driver.

usage() {
    echo "Usage: $0"
    echo "Checks the binding status of the GPU and Audio devices, and the status of the NVIDIA driver."
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
    usage
    exit 0
fi

GPU="0000:01:00.0"
AUDIO="0000:01:00.1"

echo "=== GPU Binding Status ==="
echo ""

# Check GPU binding
echo "GPU ($GPU):"
if [ -L /sys/bus/pci/devices/$GPU/driver ]; then
    DRIVER=$(readlink /sys/bus/pci/devices/$GPU/driver | awk -F '/' '{print $NF}')
    echo "  Bound to: $DRIVER"
else
    echo "  Not bound to any driver"
fi

# Check Audio binding
echo ""
echo "Audio ($AUDIO):"
if [ -L /sys/bus/pci/devices/$AUDIO/driver ]; then
    DRIVER=$(readlink /sys/bus/pci/devices/$AUDIO/driver | awk -F '/' '{print $NF}')
    echo "  Bound to: $DRIVER"
else
    echo "  Not bound to any driver"
fi

# Check NVIDIA driver status
echo ""
echo "=== NVIDIA Driver Status ==="
if lsmod | grep -q nvidia; then
    echo "NVIDIA modules loaded:"
    lsmod | grep nvidia | awk '{print "  " $1}'
else
    echo "No NVIDIA modules loaded"
fi

# Check nvidia-smi
echo ""
echo "=== NVIDIA SMI ==="
if command -v nvidia-smi &> /dev/null; then
    nvidia-smi --query-gpu=name,driver_version,pci.bus_id --format=csv,noheader 2>/dev/null || echo "nvidia-smi failed"
else
    echo "nvidia-smi not found"
fi

# Check display providers
echo ""
echo "=== Display Providers ==="
if [ -n "$DISPLAY" ]; then
    xrandr --listproviders 2>/dev/null || echo "xrandr failed (may need to run from GUI)"
else
    echo "No DISPLAY variable set (run from GUI terminal)"
fi
