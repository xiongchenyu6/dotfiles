# GPU Passthrough Quick Reference

## Quick Commands

```bash
# Check GPU status
./check_gpu_status.sh

# VM Usage
./gpu_rebind_complete.sh to-vm     # Before starting VM
./gpu_rebind_complete.sh to-host   # After stopping VM

# Fix external display
sudo ./fix_nvidia_drm.sh

# Restart Hyprland
Super+M  # Exit Hyprland (default keybind)
```

## Emergency Recovery

If GPU is stuck:
```bash
# From TTY (Ctrl+Alt+F2)
sudo modprobe -r vfio-pci
sudo modprobe nvidia
sudo modprobe nvidia_drm modeset=1
sudo systemctl restart greetd
```

## Check Everything
```bash
./check_gpu_status.sh        # GPU binding
./check_hyprland_displays.sh # Display status
nvidia-smi                   # NVIDIA status
hyprctl monitors            # Hyprland monitors
```