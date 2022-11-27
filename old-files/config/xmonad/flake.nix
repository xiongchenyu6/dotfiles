# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0
{
  description = "My haskell application";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};

      inherit (pkgs) haskellPackages;
      # DON'T FORGET TO PUT YOUR PACKAGE NAME HERE, REMOVING `throw`
      packageName = "my-xmonad";
    in {
      packages.${packageName} =
        haskellPackages.callCabal2nix (nixpkgs.lib.debug.traceVal packageName)
        (builtins.toString ./.) {
          # Dependency overrides go here
        };

      defaultPackage = self.packages.${system}.${packageName};

      devShell = pkgs.mkShell {
        buildInputs = with pkgs; [
          haskellPackages.haskell-language-server # you must build it with your ghc to work
          ghcid
          cabal-install
          xlibsWrapper
          alsaLib.dev
          xorg.libXrandr
          xorg.libXScrnSaver
        ];
        nativeBuildInputs = with pkgs; [pkg-config];
        inputsFrom = builtins.attrValues self.packages.${system};
      };
    });
}
