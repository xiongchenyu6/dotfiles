{
  modulesPath,
  inputs,
  lib,
  ezModules,
  pkgs,
  ...
}:
{
  imports = with inputs; [
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")
    ./disk-config.nix
    ezModules.root
    srvos.nixosModules.mixins-trusted-nix-caches
  ];

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
  };

  boot.loader.grub = {
    efiSupport = true;
    efiInstallAsRemovable = true;
  };

  services.openssh.enable = true;

  environment.systemPackages = map lib.lowPrio [
    pkgs.curl
    pkgs.gitMinimal
  ];

  system.stateVersion = "25.05";
}
