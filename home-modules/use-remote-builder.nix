# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ lib, ... }:
{
  home = {
    stateVersion = "26.05";
  };
  programs = {
    ssh = {
      enable = true;
      settings = {
        "*" = {
          HashKnownHosts = false;
          Compression = true;
        };
        "*.trontech.link" = {
          User = lib.mkDefault "freeman.xiong";
        };
      };
      extraConfig = ''
        PasswordAuthentication yes
      '';
    };
  };
}
