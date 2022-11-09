# Edit this configuration file to define what should be installed on

# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ hmUsers, pkgs, config, ... }: {
  users = {
    users = {
      xiongchenyu = {
        description = "freeman";
        shell = pkgs.zsh;
      };
    };
  };
  home-manager.users = { inherit (hmUsers) xiongchenyu; };
}
