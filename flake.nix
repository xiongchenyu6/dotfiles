{
  description = "just copy all the configuration files to nix storage";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }: 
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in rec {
        packages = rec {
          default = with pkgs; stdenv.mkDerivation {
            pname = "dotfiles";
            version = "0.1.0";
            src = ./.;
            installPhase = ''
              mkdir -p $out/etc;
              cp -r . $out/etc;
            '';
          };
       };
      }
    );
}
