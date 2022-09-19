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
      inputs = {
        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";
      };
    };

    emacs = {
      url = "github:nix-community/emacs-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";

      };
    };

    xddxdd = {
      url = "github:xddxdd/nur-packages";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };

    nvfetcher = {
      url = "github:berberman/nvfetcher";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };

    nix-std = {
      url = "github:chessai/nix-std";
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
    , agenix
    , nvfetcher
    , nixos-generators
    , nix-std
    , ...
    } @attrs:
      with nixpkgs;
      with lib;
      with flake-utils.lib;
      let
        pkgsFor = system: import nixpkgs { inherit system; };
        std = nix-std.lib;
      in
      rec {
        # replace 'joes-desktop' with your hostname here.
        nixosConfigurations = {
          nixos = nixosSystem {
            specialArgs = attrs;
            modules = [
              nixos-hardware.nixosModules.lenovo-thinkpad-x1-9th-gen
              nixos-hardware.nixosModules.common-gpu-intel
              self.nixosModules.bttc
              agenix.nixosModule
              ./nixos/configuration.nix
              ({ pkgs, ... }: {
                nixpkgs = {
                  overlays = map (x: x.overlay) [
                    self
                    emacs
                    agenix
                    xddxdd
                  ];
                };
              })
              home-manager.nixosModules.home-manager
              {
                home-manager = {
                  useGlobalPkgs = true;
                  useUserPackages = true;
                  users.freeman = import ./nixos/home.nix;
                };

                # Optionally, use home-manager.extraSpecialArgs to pass
                # arguments to home.nix
              }

            ];
          };
        };

        nixopsConfigurations = {
          default = rec {
            inherit nixpkgs;
            network = {
              storage = {
                legacy = {
                  databasefile = "~/.nixops/deployments.nixops";
                };
              };
              description = "Tencent cloud";
              enableRollback = true;
            };
            defaults = {
              imports = [
                agenix.nixosModule
              ];
            };
            tc =
              let
                domain = "freeman.engineer";
              in
              rec {
                _module.args = {
                  inherit domain;
                };

                imports = [
                  ./host/tc/configuration.nix
                ];

                nixpkgs = {
                  overlays = [
                    xddxdd.overlay
                  ];
                };
                deployment = {
                  targetHost = domain;
                };
              };
          };
        };
      } // eachDefaultSystem
        (system:
        let pkgs = pkgsFor system;
        in
        rec {
          packages = import ./pkgs { inherit pkgs nixos-generators; inherit (nixpkgs) lib; };

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
        nixosModules = import ./modules {
          inherit std;
        };
        templates = import ./templates;
        overlay = import ./overlay.nix {
          inherit nixos-generators; inherit (nixpkgs) lib;
        };
        libs = import ./lib;
      } //
      (
        let
          # System types to support.
          supportedSystems =
            [ "x86_64-linux" ];

          # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
          forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

          # Nixpkgs instantiated for supported system types.
          nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });

        in
        {
          hydraJobs = forAllSystems (system:
            let pkgs = nixpkgsFor.${system};
            in
            {
              "tester" = self.packages.${system}.default.overrideAttrs (prev: {
                doCheck = true;
                keepBuildDirectory = true;
                #succeedOnFailure = true;
                TESTSUITEFLAGS =
                  "NIX_DONT_SET_RPATH_x86_64_unknown_linux_gnu=1 -x -d";
                checkPhase = ''
                  echo hello
                '';
                postInstall = ''
                  echo world
                '';
                failureHook = ''
                  test -f tests/testsuite.log && cp tests/testsuite.log $out/
                  test -d tests/testsuite.dir && cp -r tests/testsuite.dir $out/
                '';
              });
              "tester-readme" = pkgs.runCommand "readme"
                { } ''
                echo hello worl
                mkdir -p $out/nix-support
                echo "# A readme" > $out/readme.md
                echo "doc readme $out/readme.md" >> $out/nix-support/hydra-build-products
              '';
            });
        }
      );
}
