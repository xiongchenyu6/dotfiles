# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ inputs, pkgs, ... }:
{
  imports = [ inputs.home-manager.darwinModules.home-manager ];
  system = {
    stateVersion = 4;
  }; # Did you read the comment?
  nix = {
    settings = {
    };
    # package = pkgs.nix;
  };
  security.pam.services.sudo_local.touchIdAuth = true;
  system.darwinLabel = "gui";
  nixpkgs.hostPlatform = "x86_64-darwin";
  nix.optimise.automatic = true;
}
