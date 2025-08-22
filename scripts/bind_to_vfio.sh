#!/usr/bin/env bash
set -euo pipefail

# This script unbinds the GPU and Audio from the current drivers and binds them to the vfio-pci driver.

usage() {
    echo "Usage: $0"
    echo "Unbinds the GPU and Audio from the current drivers and binds them to the vfio-pci driver."
}

if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
    usage
    exit 0
fi

GPU="0000:01:00.0"
AUDIO="0000:01:00.1"

echo "Unbinding GPU and Audio from current drivers..."
for DEVICE in $GPU $AUDIO; do
    if [ -L /sys/bus/pci/devices/$DEVICE/driver ]; then
        echo $DEVICE > /sys/bus/pci/devices/$DEVICE/driver/unbind
    else
        echo "Device $DEVICE is not bound to any driver."
    fi
done

echo "Binding GPU and Audio to vfio-pci driver..."
for DEVICE in $GPU $AUDIO; do
    echo $DEVICE > /sys/bus/pci/drivers/vfio-pci/bind || echo "Failed to bind $DEVICE to vfio-pci"
done

echo "Operation completed."