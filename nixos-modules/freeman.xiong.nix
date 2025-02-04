# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  pkgs,
  config,
  shares,
  lib,
  ...
}:
{
  sops.secrets."user/freeman/pass" = {
    neededForUsers = true;
  };

  users = {
    users = {
      "freeman.xiong" = {
        isNormalUser = true;
        description = "freeman.xiong";
        group = "users";

        openssh.authorizedKeys.keys = [ shares.users-dict."freeman.xiong".public-key ];
        shell = pkgs.zsh;
        hashedPasswordFile = lib.mkDefault config.sops.secrets."user/freeman/pass".path;

        extraGroups = [
          "networkmanager"
          "wheel"
          "video"
          "audio"
          "cdrom"
          "disk"
          "floppy"
          "dialout"
          "lp"
          "input"
          "docker"
          "podman"
          "tss"
          "libvirtd"
          "qemu-libvirtd"
          "kvm"
          "pulse"
          "pipewire"
          "systemd-journal"
          "adbusers"
        ];
      };
      "xiongchenyu6" = {
        isNormalUser = true;
      };
    };
  };
  programs.zsh.enable = true;
}
