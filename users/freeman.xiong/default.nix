# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ pkgs, config, profiles, lib, ... }: {
  sops.secrets."user/freeman/pass" = { neededForUsers = true; };
  imports = [ ./private-info.nix ];
  users = {
    users = {
      "freeman.xiong" = {
        isNormalUser = true;
        description = "freeman.xiong";
        group = "users";

        openssh.authorizedKeys.keys =
          [ profiles.share.users-dict."freeman.xiong".public-key ];
        shell = pkgs.zsh;

        hashedPasswordFile =
          lib.mkDefault config.sops.secrets."user/freeman/pass".path;
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
          "pulse"
          "pipewire"
        ];
      };
      "xiongchenyu6" = { isNormalUser = true; };
    };
  };
  programs.zsh.enable = true;

  home-manager = {
    users = {
      "freeman.xiong" = {
        imports = [ ../profiles/cli/common.nix ../profiles/cli/shell/zsh.nix ];
      };
    };
  };
}
