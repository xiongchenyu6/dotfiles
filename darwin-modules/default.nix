# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  inputs,
  pkgs,
  ezModules,
  ...
}:
let
  overlays =
    with inputs;
    map (x: x.overlays.default or x.overlay) [
      xiongchenyu6
      nix-alien
      sops-nix
      nix-topology
    ];
  darwin-modules = with inputs; [
    home-manager.darwinModules.home-manager
    sops-nix.darwinModules.default
    (import ../shared-modules/sops.nix)
    (_: {
      nixpkgs = {
        inherit overlays;
      };
    })
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

  security.pam.services.sudo_local.touchIdAuth = true;
  system.darwinLabel = "gui";
  nixpkgs.hostPlatform = "aarch64-darwin";
}
