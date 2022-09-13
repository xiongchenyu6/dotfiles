{
  description =
    "Flake to manage my laptop, my nur and my hosts on Tencent Cloud";

  inputs = {
    # Core Dependencies
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    flake-utils.url = "github:numtide/flake-utils";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.utils.follows = "flake-utils";
    };

    emacs = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    xddxdd = {
      url = "github:xddxdd/nur-packages";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

  };

  outputs =
    { self
    , nixpkgs
    , nixos-hardware
    , emacs
    , xddxdd
    , flake-utils
    , home-manager
    , sops-nix
    , agenix
    }:
      with nixpkgs;
      let
        pkgsFor = system: import nixpkgs { inherit system; };
        system = "x86_64-linux";
      in
      rec {
        # replace 'joes-desktop' with your hostname here.
        nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
          modules = [
            nixos-hardware.nixosModules.lenovo-thinkpad-x1-9th-gen
            nixos-hardware.nixosModules.common-gpu-intel
            #bttc.nixosModules.bttc
            agenix.nixosModule
            ./nixos/configuration.nix
            ({ pkgs, ... }: {
              nixpkgs.overlays = [
                emacs.overlay
                (final: prev: {
                  myRepo = self.packages."${prev.system}";
                  xddxdd = xddxdd.packages."${prev.system}";
                  agenix = agenix.packages."${prev.system}";
                })
              ];
            })
            home-manager.nixosModules.home-manager
            {
              home-manager.useGlobalPkgs = true;
              home-manager.useUserPackages = false;
              home-manager.users.freeman = import ./nixos/home.nix;

              # Optionally, use home-manager.extraSpecialArgs to pass
              # arguments to home.nix
            }

          ];
        };

        nixopsConfigurations = with lib; {
          default = rec {
            inherit nixpkgs;
            network.storage.legacy.databasefile = "~/.nixops/deployments.nixops";
            network.description = "Tencent cloud";
            network.enableRollback = false;
            tc =
              let
                domain = "freeman.engineer";
              in
              rec {
                _module.args = {
                  inherit domain;
                };

                imports = [
                  ./tc/configuration.nix
                  agenix.nixosModule
                  ({ pkgs, ... }: {
                    nixpkgs.overlays = [
                      (final: prev: {
                        xddxdd = xddxdd.packages."${prev.system}";
                      })
                    ];
                  })
                ];
                deployment.targetHost = domain;
              };
          };
        };

      } // flake-utils.lib.eachDefaultSystem (system:
      let pkgs = pkgsFor system;
      in
      rec {
        packages = import ./default.nix { inherit pkgs; };
        libs = import ./lib/default.nix { inherit pkgs; };
        overlays = import ./overlays/default.nix { inherit pkgs; };

        # used by nix develop and nix shell
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            # to test with nix (Nix) 2.7.0 and NixOps 2.0.0-pre-7220cbd use
            nix
            nixopsUnstable
          ];
        };
      })
      // {
        templates = import ./templates/default.nix;
      };
}
