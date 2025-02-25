{ modulesPath, ... }:
{
  imports = [ (modulesPath + "/profiles/qemu-guest.nix") ];
  boot.loader.systemd-boot = {
    enable = true;
    configurationLimit = 1;
  };
  boot.loader.efi.canTouchEfiVariables = true;
  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/42E6-0BAE";
    fsType = "vfat";
  };
  boot.initrd.availableKernelModules = [
    "ata_piix"
    "uhci_hcd"
    "xen_blkfront"
    "vmw_pvscsi"
  ];
  boot.initrd.kernelModules = [ "nvme" ];
  fileSystems."/" = {
    device = "/dev/mapper/centosvolume-root";
    fsType = "xfs";
  };

}
