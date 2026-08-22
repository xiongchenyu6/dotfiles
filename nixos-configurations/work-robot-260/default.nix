{
  inputs,
  modulesPath,
  lib,
  pkgs,
  ezModules,
  ...
}:
{
  imports = with inputs; [
    disko.nixosModules.disko
    (modulesPath + "/installer/scan/not-detected.nix")
    ezModules.root
    ezModules."freeman.xiong"
    ./disk-config.nix
    ./hardware-configuration.nix
  ];

  boot.loader.grub = {
    efiSupport = true;
    efiInstallAsRemovable = true;
  };

  services.openssh.enable = true;

  networking.networkmanager.enable = true;

  environment.systemPackages = map lib.lowPrio [
    pkgs.curl
    pkgs.gitMinimal
  ];

  system.stateVersion = "24.11";
}
