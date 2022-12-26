# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ hmUsers, pkgs, config, profiles, ... }: {
  sops.secrets."user/freeman/pass" = { neededForUsers = true; };
  imports = [ ./private-info.nix ];
  users = {
    users = {
      "freeman.xiong" = {
        isNormalUser = true;
        description = "freeman.xiong";
        group = "users";
        openssh.authorizedKeys.keys = [ profiles.share.office.user.public-key ];
        shell = pkgs.zsh;
        passwordFile = config.sops.secrets."user/freeman/pass".path;
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
        ];
      };
    };
  };
  home-manager = {
    users = {
      "freeman.xiong" =
        if (builtins.elem "with-gui" config.system.nixos.tags) then
          hmUsers.freeman-gui
        else
          hmUsers.freeman-cli;
    };
  };
}
