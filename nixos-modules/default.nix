{
  inputs,
  ezModules,
  pkgs,
  lib,
  ...
}:
let
  sharedConfig = import ../shared-modules/default.nix { inherit inputs lib; };

  nixos-modules = with inputs; [
    sops-nix.nixosModules.sops
    home-manager.nixosModules.home-manager
    nur.modules.nixos.default
    impermanence.nixosModules.impermanence
    nix-topology.nixosModules.default
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    (sharedConfig.mkNixosNixpkgsConfig sharedConfig.nixosOverlays)
  ];
in
{
  imports = [
    ezModules.kernel
    ezModules.security
    ezModules.ssh-harden
    ../shared-modules/core.nix
    ../shared-modules/sops.nix
  ]
  ++ nixos-modules;

  home-manager = sharedConfig.homeManagerConfig;

  zramSwap.enable = true;
  boot.kernel.sysctl = {
    "vm.swappiness" = 10;
  };
  environment = {
    systemPackages = with pkgs; [
      lrzsz
    ];
  };
  services = {
    resolved = {
      enable = true;
      # dnssec = "allow-downgrade";
      # dnsovertls = "opportunistic";
      # llmnr = "false";
    };

  };
  networking.nftables.enable = true;
}
