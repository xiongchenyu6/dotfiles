# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  inputs,
  pkgs,
  ezModules,
  ...
}:
{
  imports = [
    inputs.home-manager.darwinModules.home-manager
    inputs.sops-nix.darwinModules.default
    ezModules.client-cli
    ezModules.client-gui
    ../shared-modules/core.nix
    ../shared-modules/sops.nix
  ];

  system = {
    stateVersion = 6;
  };

  security.pam.services.sudo_local.touchIdAuth = true;
  system.darwinLabel = "gui";
  nixpkgs.hostPlatform = "x86_64-darwin";
  nix.optimise.automatic = true;

  # Add additional configurations here
  environment.systemPackages = with pkgs; [
    vim
    git
    # Add more packages as needed
  ];

  services = {
    # Enable and configure services here
  };

}
