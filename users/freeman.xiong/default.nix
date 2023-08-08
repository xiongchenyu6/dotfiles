# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ hmUsers, pkgs, config, profiles, lib, ... }: {
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
        passwordFile =
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
    };
  };
  home-manager = {
    users = {
      "freeman.xiong" =
        if (builtins.elem "with-gui-nvidia" config.system.nixos.tags) then
          hmUsers.freeman-hyprland-nvidia
          # hmUsers.freeman-hyprland
          # hmUsers.freeman-xmonad
        else if (builtins.elem "with-gui" config.system.nixos.tags) then
          hmUsers.freeman-hyprland
          # hmUsers.freeman-xmonad
        else
          hmUsers.freeman-cli;
    };
  };
}
