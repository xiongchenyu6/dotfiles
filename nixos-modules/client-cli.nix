# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ pkgs, ... }:
{
  environment = {
    systemPackages = with pkgs; [
    ];

    pathsToLink = [ "/share/zsh" ];
  };
  services.pcscd.enable = true;

  programs = {
    npm = {
      enable = true;
    };
    nix-ld.enable = true;
  };
}
