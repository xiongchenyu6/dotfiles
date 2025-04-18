# This module defines a small NixOS installation CD. It does not
# contain any graphical stuff.
{
  pkgs,
  lib,
  modulesPath,
  ...
}:
{
  imports = [
    # Currently fails to build due to ZFS incompatibility with bcachefs
    #<nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
    (modulesPath + "/installer/cd-dvd/installation-cd-graphical-calamares-gnome.nix")
  ];
  boot.supportedFilesystems = {
    btrfs = true;
    zfs = lib.mkForce false;
    bcachefs = true;
  };

  nixpkgs.hostPlatform = "x86_64-linux";

  environment = {
    systemPackages = with pkgs; [
      gnupg
      sops
    ];
  };
  nix = {
    settings = {
      accept-flake-config = true;
      allow-import-from-derivation = true;
      experimental-features = [
        "nix-command"
        "flakes"
        "ca-derivations"
        "parse-toml-timestamps"
      ];
      trusted-users = [
        "freeman.xiong"
        "freeman"
        "@wheel"
        "@admin"
      ];
      allowed-users = [
        "root"
        "freeman"
        "freeman.xiong"
        "@wheel"
        "@admin"
      ];
      substituters = [
        "https://xddxdd.cachix.org"
        "https://xiongchenyu6.cachix.org"
        "https://hyprland.cachix.org"
      ];
      trusted-public-keys = [
        "xddxdd.cachix.org-1:ay1HJyNDYmlSwj5NXQG065C8LfoqqKaTNCyzeixGjf8="
        "xiongchenyu6.cachix.org-1:mpOGlINmMwc2gb3xb1BjVmhzR8BYWzWYlg4xlTiBr7Q="
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
      ];
    };
    distributedBuilds = lib.mkDefault true;
  };

  # kernelPackages already defined in installation-cd-minimal-new-kernel-no-zfs.nix
  boot.kernelPackages = lib.mkOverride 0 pkgs.linuxPackages_latest;
  isoImage.squashfsCompression = "gzip -Xcompression-level 1";
}
