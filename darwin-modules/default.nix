# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ inputs, pkgs, ... }:
let
  darwin-modules = [ inputs.home-manager.darwinModules.home-manager ];
in
{

  environment.systemPackages = with pkgs; [
    krb5
    openssh_gssapi
  ];

  imports = [ ./common.nix ] ++ darwin-modules;
  services.nix-daemon.enable = true;
  system = {
    stateVersion = 4;
  }; # Did you read the comment?
  nix = {
    settings = {
      auto-optimise-store = true;
    };
    # package = pkgs.nix;
  };
  security.pam.enableSudoTouchIdAuth = true;

  system.darwinLabel = "gui";
}
