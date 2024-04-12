# This module defines a small NixOS installation CD. It does not
# contain any graphical stuff.
{ pkgs, lib, modulesPath, ... }: {
  imports = [
    # Currently fails to build due to ZFS incompatibility with bcachefs
    #<nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-minimal.nix>
    (modulesPath + "/installer/cd-dvd/installation-cd-graphical-gnome.nix")
  ];
  boot.supportedFilesystems = {
    btrfs = true;
    zfs = lib.mkForce false;
    bcachefs = true;
  };

  nixpkgs.hostPlatform = "x86_64-linux";

  # kernelPackages already defined in installation-cd-minimal-new-kernel-no-zfs.nix
  boot.kernelPackages = lib.mkOverride 0 pkgs.linuxPackages_latest;
  isoImage.squashfsCompression = "gzip -Xcompression-level 1";
}
