{
  inputs,
  ezModules,
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
      poetry2nix
      nix-topology
    ];

  sharedOverlays = overlays ++ [
  ];

  nixos-modules = with inputs; [
    sops-nix.nixosModules.sops
    home-manager.nixosModules.home-manager
    nur.modules.nixos.default
    impermanence.nixosModules.impermanence
    nix-topology.nixosModules.default
    (import ../shared-modules/sops.nix)
    (_: {
      nixpkgs = {
        system = lib.mkDefault "x86_64-linux";
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
  ] ++ nixos-modules;

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
  };

  services.resolved = {
    enable = true;
    # dnssec = "allow-downgrade";
    # dnsovertls = "opportunistic";
    # llmnr = "false";
  };
}
