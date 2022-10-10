{ config, pkgs, options, lib, ... }:
let
  common-files-path = ../../common;
  share = import (common-files-path + /share.nix);
in

{
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

    users.root = {
      openssh.authorizedKeys.keys = [
        share.office.user.public-key
      ];
    };

    users = {
      freeman = {
        isNormalUser = true;
        description = "freeman";
        group = "users";
        openssh.authorizedKeys.keys = [
          share.office.user.public-key
        ];
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
  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users = {
      freeman = {
        home = {
          stateVersion = "22.11";
        };
        imports = [ ../../home/cli ];
      };
    };
  };

}
