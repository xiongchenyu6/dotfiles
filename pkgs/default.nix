# This file describes your repository contents.
# It should return a set of nix derivations
# and optionally the special attributes `lib`, `modules` and `overlays`.
# It should NOT import <nixpkgs>. Instead, you should take pkgs as an argument.
# Having pkgs default to <nixpkgs> is fine though, and it lets you use short
# commands such as:
#     nix-build -A mypackage

{ pkgs, nixos-generators, lib, ... }:

with pkgs;
with builtins;
let
  source = callPackage ./_sources/generated.nix {
    inherit fetchFromGitHub fetchurl fetchgit;
  };
  allPkgs = my-pkgs // pkgs // { inherit source; };
  callPackage = lib.callPackageWith allPkgs;
  my-pkgs = rec {
    example-package = callPackage ./example-package { };
    bttc = callPackage ./bttc { };
    delivery = callPackage ./delivery { };
    my_cookies = callPackage ./python3/my_cookies { };

    vbox = nixos-generators.nixosGenerate {
      system = system;
      format = "virtualbox";
    };
    amazon = nixos-generators.nixosGenerate {
      system = "x86_64-linux";
      format = "amazon";
    };
    # dotfiles = with pkgs;
    #   stdenv.mkDerivation {
    #     pname = "dotfiles";
    #     version = "0.1.0";
    #     src = ./.;
    #     installPhase = ''
    #       mkdir -p $out/etc;
    #       cp -r . $out/etc;
    #     '';
    #   };
    # default = dotfiles;
    # };
  };
in
my-pkgs
