{ config, pkgs, lib, ... }:
let
  nvidia-offload = pkgs.writeShellScriptBin "nvidia-offload" ''
    export __NV_PRIME_RENDER_OFFLOAD=1
    export __NV_PRIME_RENDER_OFFLOAD_PROVIDER=NVIDIA-G0
    export __GLX_VENDOR_LIBRARY_NAME=nvidia
    export __VK_LAYER_NV_optimus=NVIDIA_only
    exec "$@"
  '';
  kver = config.boot.kernelPackages.kernel.version;
in {
  services.fstrim.enable = lib.mkDefault true;
  hardware.cpu.amd.updateMicrocode =
    lib.mkDefault config.hardware.enableRedistributableFirmware;
  boot = lib.mkMerge [
    (lib.mkIf
      ((lib.versionAtLeast kver "5.17") && (lib.versionOlder kver "6.1")) {
        kernelParams = [ "initcall_blacklist=acpi_cpufreq_init" ];
        kernelModules = [ "amd-pstate" ];
      })
    (lib.mkIf (lib.versionAtLeast kver "6.1") {
      kernelParams = [ "amd_pstate=passive" ];
    })
  ];
  # hardware.nvidia.powerManagement.finegrained = true;
  hardware.nvidia.modesetting.enable = true;

  services.xserver.videoDrivers = [ "nvidia" "amdgpu" "modesetting" ];
  hardware.opengl = {
    driSupport = lib.mkDefault true;
    driSupport32Bit = lib.mkDefault true;
  };

  hardware.opengl.extraPackages = with pkgs; [
    amdvlk
    driversi686Linux.amdvlk
    rocm-opencl-icd
    rocm-opencl-runtime
    vaapiVdpau
  ];
  environment.systemPackages = [ nvidia-offload ];

  hardware.nvidia.prime = { amdgpuBusId = lib.mkForce "PCI:5:0:0"; };
  hardware.nvidia.prime = {
    reverseSync.enable = true;
    # offload.enable = true;
    # sync.enable = true;
    nvidiaBusId = "PCI:1:0:0";
  };
}
