{ lib, modulesPath, ... }:
{
  imports = [
    ./base.nix
    "${modulesPath}/virtualisation/digital-ocean-image.nix"
  ];
  boot.loader.grub.devices = lib.mkForce [ "/dev/vda" ];
  boot.loader.grub.device = "/dev/vda";
  virtualisation.digitalOceanImage.compressionMethod = "bzip2";
}
