# Shared configuration factory for both Darwin and NixOS modules
# This can be imported and used to generate consistent configuration
{
  inputs,
  lib,
}:
let
  # Common overlays shared between Darwin and NixOS
  baseOverlays =
    with inputs;
    map (x: x.overlays.default or x.overlay) [
      xiongchenyu6
      nix-alien
      sops-nix
      nix-topology
      openclaw
    ];

  # Additional overlays for NixOS
  nixosAdditionalOverlays =
    with inputs;
    map (x: x.overlays.default or x.overlay) [
      nur
      rust-web-server
    ];
in
{
  inherit baseOverlays nixosAdditionalOverlays;

  # Combined overlays for NixOS
  nixosOverlays =
    baseOverlays
    ++ nixosAdditionalOverlays
    ++ [
      (
        _: prev:
        {
          # gnupg240 = nixpkgs-stable.legacyPackages.x86_64-linux.gnupg;
          # telegram-desktop =
          #   nixpkgs-stable.legacyPackages.x86_64-linux.telegram-desktop;
          # waybar = nixpkgs-master.legacyPackages.x86_64-linux.waybar;
          netbird = prev.netbird.override {
            buildGoModule = prev.buildGo125Module;
          };
        }
        // lib.optionalAttrs (prev.stdenv.hostPlatform.system == "x86_64-linux") {
          microsoft-edge = inputs.nixpkgs-stable.legacyPackages.x86_64-linux.microsoft-edge;
        }
      )
    ];

  # Home Manager configuration shared between Darwin and NixOS
  homeManagerConfig = {
    useGlobalPkgs = true;
    useUserPackages = true;
    backupFileExtension = "backup";
  };

  # Generate nixpkgs configuration with overlays
  mkNixpkgsConfig =
    overlays:
    (_: {
      nixpkgs = {
        inherit overlays;
      };
    });

  # Generate nixpkgs configuration with overlays and hostPlatform for NixOS
  mkNixosNixpkgsConfig =
    overlays:
    (_: {
      nixpkgs = {
        hostPlatform = lib.mkDefault "x86_64-linux";
        inherit overlays;
      };
    });
}
