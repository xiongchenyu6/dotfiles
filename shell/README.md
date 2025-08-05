# GPU Passthrough Scripts for NixOS with Hyprland

This collection of scripts helps manage NVIDIA GPU passthrough between host (NixOS with Hyprland) and virtual machines (VMs).

## Prerequisites

1. **NixOS Configuration**:
   ```nix
   # In your configuration.nix
   boot.kernelParams = [ 
     "intel_iommu=on" # or "amd_iommu=on" for AMD
     "iommu=pt"
     "nvidia-drm.modeset=1"
   ];
   
   hardware.nvidia = {
     modesetting.enable = true;
     powerManagement.enable = true;
   };
   
   virtualisation.libvirtd.enable = true;
   ```

2. **Hyprland Environment Variables** (in home-manager):
   ```nix
   wayland.windowManager.hyprland = {
     settings = {
       env = [
         "WLR_NO_HARDWARE_CURSORS,1"
         "__GLX_VENDOR_LIBRARY_NAME,nvidia"
         "GBM_BACKEND,nvidia-drm"
       ];
     };
   };
   ```

## Scripts Overview

### Core GPU Management

#### `bind_to_vfio.sh`
Binds NVIDIA GPU to vfio-pci driver for VM passthrough.
```bash
sudo ./bind_to_vfio.sh
```

#### `bind_gpu_to_host_hyprland.sh`
Rebinds NVIDIA GPU back to host system with Hyprland-specific fixes.
```bash
sudo ./bind_gpu_to_host_hyprland.sh
```

#### `gpu_rebind_complete.sh`
Complete workflow script for GPU passthrough management.
```bash
# Bind GPU to VM
./gpu_rebind_complete.sh to-vm

# Bind GPU back to host
./gpu_rebind_complete.sh to-host
```

### Diagnostic Tools

#### `check_gpu_status.sh`
Shows current GPU binding status and driver information.
```bash
./check_gpu_status.sh
```

#### `check_hyprland_displays.sh`
Displays Hyprland monitor configuration and DRM device status.
```bash
./check_hyprland_displays.sh
```

#### `check_iommu.sh`
Verifies IOMMU groups for GPU passthrough compatibility.
```bash
./check_iommu.sh
```

### Utility Scripts

#### `fix_nvidia_drm.sh`
Fixes NVIDIA DRM module loading issues (run as root).
```bash
sudo ./fix_nvidia_drm.sh
```

#### `restart_hyprland.sh`
Gracefully restarts Hyprland compositor.
```bash
./restart_hyprland.sh
```

## Typical Workflow

### 1. Starting a VM with GPU Passthrough

```bash
# Check current GPU status
./check_gpu_status.sh

# Bind GPU to VFIO for VM
./gpu_rebind_complete.sh to-vm

# Start your VM with GPU passthrough
virsh start your-vm-name
```

### 2. Returning GPU to Host

```bash
# Shutdown the VM
virsh shutdown your-vm-name

# Wait for VM to fully stop
virsh list --all

# Rebind GPU to host
./gpu_rebind_complete.sh to-host

# If external display doesn't work, run:
sudo ./fix_nvidia_drm.sh

# Restart Hyprland (choose one):
# Option 1: Press Super+M to exit and re-login
# Option 2: From another TTY (Ctrl+Alt+F2):
sudo systemctl restart greetd
```

## Troubleshooting

### External Display Not Working After Rebind

1. Check if nvidia_drm is loaded:
   ```bash
   lsmod | grep nvidia_drm
   ```

2. If not loaded, run:
   ```bash
   sudo ./fix_nvidia_drm.sh
   ```

3. Verify DRM devices:
   ```bash
   ls -la /dev/dri/
   # Should show both card0 (NVIDIA) and card1 (AMD/Intel)
   ```

4. Check Hyprland displays:
   ```bash
   ./check_hyprland_displays.sh
   ```

### GPU Not Unbinding

If GPU fails to unbind, check if any processes are using it:
```bash
sudo lsof /dev/nvidia*
nvidia-smi
```

### IOMMU Issues

Verify IOMMU is enabled:
```bash
./check_iommu.sh
dmesg | grep -i iommu
```

## Important Notes

- Always shutdown VMs before rebinding GPU to host
- Hyprland requires a full restart (not just reload) after GPU changes
- The `nvidia_drm` module with `modeset=1` is essential for Wayland
- External displays may take a few seconds to be detected after rebind

## Hardware IDs

Default GPU IDs in scripts (modify if yours differ):
- GPU: `0000:01:00.0`
- Audio: `0000:01:00.1`

Find your GPU IDs:
```bash
lspci | grep -E "VGA|3D|Display|Audio.*NVIDIA"
```

## License

These scripts are provided as-is for the NixOS community.