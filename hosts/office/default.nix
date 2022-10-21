# Edit

{ config, pkgs, options, lib, ... }: rec {
  networking = { hostName = "office"; };

  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../../nixos
    ../../nixos/client.nix
  ];
  boot = {
    tmpOnTmpfs = lib.mkDefault true;
    loader = {
      systemd-boot = {
        enable = true;
        editor = false;
      };
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/efi";
      };
    };
  };
}
