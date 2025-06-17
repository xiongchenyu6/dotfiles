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
        uid = 1000;
        openssh.authorizedKeys.keys = [
          shares.users-dict."freeman.xiong".public-key
          shares.users-dict."freeman.xiong".yubikey
          "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBOCfF/bo5cgjjwlFrsDH63nyo2kf+byktzVWR6FRhtNNqmGqOl5Ze/zBfNlk8+rp5zRVPXjO5PWLezHydgbbUxY= iPhone Termius"
        ];
        shell = pkgs.zsh;
        hashedPasswordFile = lib.mkDefault config.sops.secrets."user/freeman/pass".path;
        home = "/home/freeman.xiong";
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
    };
  };
}
