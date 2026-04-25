# Shared configuration factory for both Darwin and NixOS modules
# This can be imported and used to generate consistent configuration
{
  inputs,
  lib,
}:
let
  masterPkgsFor =
    system:
    import inputs.nixpkgs-master {
      inherit system;
      config.allowUnfree = true;
    };

  # Common overlays shared between Darwin and NixOS
  baseOverlays =
    with inputs;
    map (x: x.overlays.default or x.overlay) [
      xiongchenyu6
      nix-alien
      sops-nix
      nix-topology
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
          claude-code = (masterPkgsFor prev.stdenv.hostPlatform.system).claude-code;
          netbird = prev.netbird.override {
            buildGoModule = prev.buildGo125Module;
          };
        }
        // lib.optionalAttrs (prev.stdenv.hostPlatform.system == "x86_64-linux") {
          microsoft-edge = inputs.nixpkgs-stable.legacyPackages.x86_64-linux.microsoft-edge;
        }
      )
      # nixpkgs-unstable as of 2026-04-25 ships python3.13-cli-helpers 2.10.0
      # whose `tests/tabular_output/test_preprocessors.py::test_style_output*`
      # tests assert ANSI escapes equal `\x1b[39m` but newer Pygments emits
      # the joined `\x1b[39;49m` form. Cosmetic test breakage; output is
      # still functional. Drop doCheck until upstream fixes the assertion.
      # Affects pgcli + litecli (both depend on cli-helpers).
      (_: prev: {
        python313Packages = prev.python313Packages.overrideScope (
          _: pyprev: {
            cli-helpers = pyprev.cli-helpers.overridePythonAttrs (_: {
              doCheck = false;
            });
          }
        );
      })
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
