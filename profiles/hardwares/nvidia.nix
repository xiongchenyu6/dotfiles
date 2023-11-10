{ config, pkgs, lib, ... }:
{

  # services.fstrim.enable = lib.mkDefault true;
  # hardware.cpu.amd.updateMicrocode =
  #   lib.mkDefault config.hardware.enableRedistributableFirmware;
  # boot = lib.mkMerge [
  #   (lib.mkIf
  #     ((lib.versionAtLeast kver "5.17") && (lib.versionOlder kver "6.1")) {
  #       kernelParams = [ "initcall_blacklist=acpi_cpufreq_init" ];
  #       kernelModules = [ "amd-pstate" ];
  #     })
  #   (lib.mkIf (lib.versionAtLeast kver "6.1") {
  #     kernelParams = [ "amd_pstate=passive" ];
  #   })
  # ];
  # hardware.nvidia.powerManagement.finegrained = true;
  # hardware.nvidia.modesetting.enable = true;

  # services.xserver.videoDrivers = [ "nvidia" "amdgpu" "modesetting" ];
  # hardware.opengl = {
  #   driSupport = lib.mkDefault true;
  #   driSupport32Bit = lib.mkDefault true;
  # };

  # hardware.opengl.extraPackages = with pkgs; [
  #   amdvlk
  #   driversi686Linux.amdvlk
  #   rocm-opencl-icd
  #   rocm-opencl-runtime
  #   vaapiVdpau
  # ];
  # environment.systemPackages = [ nvidia-offload ];

  hardware.nvidia.prime = { amdgpuBusId = lib.mkForce "PCI:5:0:0"; };
  # hardware.nvidia.prime = {
  #   # reverseSync.enable = true;
  #   offload.enable = true;
  #   # sync.enable = true;
  #   nvidiaBusId = "PCI:1:0:0";
  # };
  services = {
    greetd = {
      settings = {
        initial_session = let
          s = pkgs.writeShellApplication {
            name = "s.sh";
            runtimeInputs = [ pkgs.hyprland ];
            text = ''
              export WLR_NO_HARDWARE_CURSORS=1;
              export LIBVA_DRIVER_NAME=nvidia
              export XDG_SESSION_TYPE=wayland
              export GBM_BACKEND=nvidia-drm
              export __GLX_VENDOR_LIBRARY_NAME=nvidia
              export _JAVA_AWT_WM_NONREPARENTING=1
              export XCURSOR_SIZE=24
              exec Hyprland
            '';
          };
        in { command = "${s}/bin/s.sh"; };
      };
    };
  };
}
