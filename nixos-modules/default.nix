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
      foundry
      poetry2nix
      nix-topology
    ];

  sharedOverlays = overlays ++ [
    (_: prev: {
      # gnupg240 = nixpkgs-stable.legacyPackages.x86_64-linux.gnupg;
      # telegram-desktop =
      #   nixpkgs-stable.legacyPackages.x86_64-linux.telegram-desktop;
      # waybar = nixpkgs-master.legacyPackages.x86_64-linux.waybar;
      www_dist = inputs.autolife_www.packages.x86_64-linux.dist;
    })
    (self: super: {
      coturn = super.coturn.overrideAttrs (oldAttrs: rec {
        postFixup =
          ''
            # Remove dangling symlinks in the examples directory.
            find $out/share/examples/turnserver/etc -xtype l -delete || true
          ''
          + (oldAttrs.postFixup or "");
      });
    })
    (self: super: {
      pythonPackages = super.pythonPackages // {
        duckduckgo-search = super.pythonPackages.duckduckgo-search.overrideAttrs (oldAttrs: {
          nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ]) ++ [
            (super.runCommand "disable-deps-check" { } "echo 'Disabling pythonRuntimeDepsCheckHook'")
          ];
          dontCheckRuntimeDeps = true;
        });
      };
    })
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

  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = true;
    android_sdk.accept_license = true;
    permittedInsecurePackages = [
      "dotnet-sdk-6.0.428"
    ];
  };

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
