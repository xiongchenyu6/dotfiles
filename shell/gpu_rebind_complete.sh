#!/usr/bin/env bash

# Complete GPU rebind workflow for Hyprland

if [ "$1" == "to-vm" ]; then
    echo "Binding GPU to VM..."
    sudo ./bind_to_vfio.sh
    echo "GPU bound to VFIO for VM passthrough"
    
elif [ "$1" == "to-host" ]; then
    echo "Binding GPU back to host..."
    
    # First, make sure VM is not running
    if virsh list --all | grep -q running; then
        echo "WARNING: VMs are still running. Please shut them down first."
        exit 1
    fi
    
    # Run the rebind script
    sudo ./bind_gpu_to_host_hyprland.sh
    
    echo ""
    echo "GPU rebound to host. Now you need to restart Hyprland:"
    echo "1. Press Super+M to exit Hyprland"
    echo "2. Log back in"
    echo ""
    echo "Or from another TTY (Ctrl+Alt+F2):"
    echo "  sudo systemctl restart greetd"
    
else
    echo "Usage: $0 [to-vm|to-host]"
    echo "  to-vm   - Bind GPU to VFIO for VM passthrough"
    echo "  to-host - Bind GPU back to host system"
fi