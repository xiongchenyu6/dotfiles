{ lib, modulesPath, ... }:
{
  imports = [
    #    ./base.nix
    "${modulesPath}/virtualisation/digital-ocean-image.nix"
  ];
  boot.loader.grub.devices = lib.mkForce [ "/dev/vda" ];
  boot.loader.grub.device = "/dev/vda";
  swapDevices = [
    {
      device = "/swap/swapfile";
      size = 1024 * 2; # 2 GB
    }
  ];

  system.stateVersion = "24.05"; # Never change this
  # virtualisation.digitalOceanImage.compressionMethod = "bzip2";
}
