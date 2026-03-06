# Shared Darwin base — imported by all macOS hosts
{
  inputs,
  pkgs,
  ezModules,
  lib,
  ...
}:
let
  sharedConfig = import ../shared-modules/default.nix { inherit inputs lib; };

  darwin-modules = with inputs; [
    home-manager.darwinModules.home-manager
    sops-nix.darwinModules.default
    (sharedConfig.mkNixpkgsConfig sharedConfig.baseOverlays)
  ];
in
{
  imports = [
    ezModules.client-cli
    ezModules.client-gui
    ../shared-modules/core.nix
    ../shared-modules/sops.nix
  ] ++ darwin-modules;

  system = {
    stateVersion = 6;
  };

  home-manager = sharedConfig.homeManagerConfig;

  security.pam.services.sudo_local.touchIdAuth = true;
}
