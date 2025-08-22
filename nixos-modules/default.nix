{
  inputs,
  ezModules,
  pkgs,
  lib,
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
      nur
      rust-web-server
    ];

  sharedOverlays = overlays ++ [
    (_: prev: {
      # gnupg240 = nixpkgs-stable.legacyPackages.x86_64-linux.gnupg;
      # telegram-desktop =
      #   nixpkgs-stable.legacyPackages.x86_64-linux.telegram-desktop;
      # waybar = nixpkgs-master.legacyPackages.x86_64-linux.waybar;
      microsoft-edge = inputs.nixpkgs-stable.legacyPackages.x86_64-linux.microsoft-edge;
    })
  ];

  nixos-modules = with inputs; [
    sops-nix.nixosModules.sops
    home-manager.nixosModules.home-manager
    nur.modules.nixos.default
    impermanence.nixosModules.impermanence
    nix-topology.nixosModules.default
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    (import ../shared-modules/sops.nix)
    (_: {
      nixpkgs = {
        hostPlatform = lib.mkDefault "x86_64-linux";
        overlays = sharedOverlays;
      };
    })
  ];
in
{
  imports = [
    ezModules.kernel
    ezModules.security
    ezModules.ssh-harden
  ]
  ++ nixos-modules;

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    backupFileExtension = "backup";
  };

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
    # kanidm = {
    #   package = pkgs.kanidmWithSecretProvisioning;
    # };

    resolved = {
      enable = true;
      # dnssec = "allow-downgrade";
      # dnsovertls = "opportunistic";
      # llmnr = "false";
    };

  };
  networking.nftables.enable = true;
}
