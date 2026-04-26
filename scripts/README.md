# GPU Passthrough Scripts for NixOS

Scripts for managing NVIDIA GPU passthrough between the host (NixOS) and
KVM/libvirt VMs.

> **Compositor note.** The `game` workstation now runs **niri**. The
> compositor-agnostic IOMMU/VFIO bind logic (`bind_to_vfio.sh`,
> `check_iommu.sh`, `check_gpu_status.sh`, `fix_nvidia_drm.sh`) is still
> current. The scripts whose names contain `hyprland` (and the Hyprland
> branches in `gpu_rebind_complete.sh`) call `hyprctl` directly and have
> not yet been ported to niri — see [Migrating to niri](#migrating-to-niri)
> below.

## Prerequisites

1. **NixOS configuration**:
   ```nix
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

2. **NVIDIA Wayland environment variables** — passed to the compositor's
   environment:
   ```nix
   # niri (active on `game`)
   programs.niri.settings.environment = {
     WLR_NO_HARDWARE_CURSORS    = "1";
     __GLX_VENDOR_LIBRARY_NAME  = "nvidia";
     GBM_BACKEND                = "nvidia-drm";
   };

   # Hyprland (legacy, retained for the Hyprland-specific scripts)
   wayland.windowManager.hyprland.settings.env = [
     "WLR_NO_HARDWARE_CURSORS,1"
     "__GLX_VENDOR_LIBRARY_NAME,nvidia"
     "GBM_BACKEND,nvidia-drm"
   ];
   ```

## Scripts

### Compositor-agnostic

| Script | Purpose |
|---|---|
| `bind_to_vfio.sh` | Bind NVIDIA GPU to `vfio-pci` for VM passthrough |
| `check_gpu_status.sh` | Show current GPU driver binding |
| `check_iommu.sh` | Verify IOMMU groups for passthrough |
| `fix_nvidia_drm.sh` | Reload `nvidia_drm` after rebind (run as root) |

### Hyprland-specific (legacy)

These scripts call `hyprctl` and assume a running Hyprland session. They
need to be ported (or replaced) for the current niri workstation.

| Script | Purpose | niri equivalent |
|---|---|---|
| `bind_gpu_to_host_hyprland.sh` | Rebind GPU to host + restart Hyprland | TODO: port to `niri msg action quit` + greetd restart |
| `check_hyprland_displays.sh` | Print Hyprland monitor config | `niri msg outputs` |
| `restart_hyprland.sh` | Graceful Hyprland restart | `systemctl restart greetd` from another TTY |
| `gpu_rebind_complete.sh` | One-shot `to-vm` / `to-host` workflow | Hyprland branch in `to-host` needs porting |

## Workflow

### 1. Start a VM with GPU passthrough

```bash
./check_gpu_status.sh                       # see current state
./gpu_rebind_complete.sh to-vm              # bind GPU → vfio-pci
virsh start your-vm-name
```

### 2. Return GPU to the host

```bash
virsh shutdown your-vm-name
virsh list --all                            # wait for the VM to fully stop
./gpu_rebind_complete.sh to-host            # ⚠️  restarts Hyprland — see note below
sudo ./fix_nvidia_drm.sh                    # if the external display stays dark
```

> **niri users:** `gpu_rebind_complete.sh to-host` triggers
> `bind_gpu_to_host_hyprland.sh`, which won't work under niri. Until that
> script is ported, run the bind portion manually
> (`bind_to_vfio.sh`-equivalent rebind to `nvidia`) and restart your niri
> session via `systemctl restart greetd` from another TTY.

## Troubleshooting

### External display stays dark after rebind

```bash
lsmod | grep nvidia_drm                     # confirm module is loaded
sudo ./fix_nvidia_drm.sh                    # reload if missing
ls -la /dev/dri/                            # should show card0 (NVIDIA) + card1 (iGPU)
```

### GPU refuses to unbind

```bash
sudo lsof /dev/nvidia*
nvidia-smi
```

### IOMMU not working

```bash
./check_iommu.sh
dmesg | grep -i iommu
```

## Hardware IDs

Default GPU IDs hardcoded in the scripts (edit if yours differ):

- GPU: `0000:01:00.0`
- HDMI audio: `0000:01:00.1`

```bash
lspci | grep -E "VGA|3D|Display|Audio.*NVIDIA"
```

## Migrating to niri

The Hyprland-specific scripts can mostly be ported by swapping:

| Hyprland | niri equivalent |
|---|---|
| `hyprctl monitors` | `niri msg outputs` |
| `hyprctl activewindow` | `niri msg focused-window` |
| `hyprctl dispatch exit` | `niri msg action quit` |
| `pkill -9 Hyprland` | `pkill -9 niri` |

A full port also needs to update the env-var injection step (niri reads
from `programs.niri.settings.environment`, not Hyprland's `env =`
list-of-strings format).

## Notes

- Always shut down VMs before rebinding GPU to host.
- The `nvidia_drm` module with `modeset=1` is essential for Wayland.
- External displays may take a few seconds to be detected after rebind.
