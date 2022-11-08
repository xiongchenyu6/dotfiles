# Edit this configuration file to define what should be installed on

# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ hmUsers, pkgs, config, ... }:
let
  common-files-path = ../common;
  share = import (common-files-path + /share.nix);

in {
  users = {
    # Define a user account. Don't forget to set a password with ‘passwd’.
    motd = ''

                                Welcome to:
       ______.      ___       ______.    ______.  .__  ___    ___
      /___   /     /   \     |   _   \  |   _   \ |__| \  \  /  /
         /  /     /  .  \    |  <_>  /  |  <_>  / |  |  \  \/  /
        /  /     /  /-\  \   |   _  .   |   _  .  |  |   >    <
       /  /__.  /  /---\  \  |  <_>  \  |  <_>  \ |  |  /  /\  \
      /._____/ /__/     \__\ |______./  |______./ |__| /__/  \_ \
                                                               \/


    '';

    users = {
      xiongchenyu = {
        isNormalUser = true;
        description = "freeman";
        group = "users";
        openssh.authorizedKeys.keys = [ share.office.user.public-key ];
        shell = pkgs.zsh;
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
        ];
      };
    };
  };
  home-manager.users = {
    xiongchenyu = hmUsers.freeman-gui;
  };
}
