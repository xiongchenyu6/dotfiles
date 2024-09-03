{
  # nixConfig.extra-experimental-features = "nix-command flakes";

  description = "Flake to manage my laptop, my nur and my hosts on Tencent Cloud";

  inputs = {

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    nur.url = "github:nix-community/NUR";

    flake-utils.url = "github:numtide/flake-utils";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };

    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL/main";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };

    nix-vscode-extensions = {
      url = "github:nix-community/nix-vscode-extensions";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };
    flake-parts.url = "github:hercules-ci/flake-parts";

    xiongchenyu6 = {
      url = "github:xiongchenyu6/nur-packages";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
      };
    };

    impermanence.url = "github:nix-community/impermanence";

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };

    nix-alien = {
      url = "github:thiagokokada/nix-alien";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    foundry = {
      url = "github:shazow/foundry.nix/monthly";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    poetry2nix = {
      url = "github:nix-community/poetry2nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    vscode-server = {
      url = "github:nix-community/nixos-vscode-server";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    authentik-nix = {
      url = "github:nix-community/authentik-nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
    };
    robot_signal_dashboard = {
      url = "git+ssh://git@github.com/AutoLifeRobot/robot_signal_dashboard.git";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
    };

  };

  outputs =
    {
      self,
      nixpkgs,
      impermanence,
      nur,
      nixos-hardware,
      home-manager,
      flake-parts,
      pre-commit-hooks,
      nix-alien,
      xiongchenyu6,
      sops-nix,
      foundry,
      poetry2nix,
      nix-vscode-extensions,
      nixos-wsl,
      vscode-server,
      disko,
      authentik-nix,
      robot_signal_dashboard,
      ...
    }@inputs:
    with nixpkgs;
    with lib;
    let
      overlays = map (x: x.overlays.default or x.overlay) [
        xiongchenyu6
        nix-alien
        sops-nix
        foundry
        poetry2nix
        nix-vscode-extensions
      ];

      sharedOverlays = overlays ++ [
        (_: prev: {
          lib = prev.lib.extend (_lfinal: _lprev: { mine = import ./lib { inherit lib; }; });
          # gnupg240 = nixpkgs-stable.legacyPackages.x86_64-linux.gnupg;
          # telegram-desktop =
          #   nixpkgs-stable.legacyPackages.x86_64-linux.telegram-desktop;
          # waybar = nixpkgs-master.legacyPackages.x86_64-linux.waybar;
        })
      ];
      darwin-modules = [ home-manager.darwinModules.home-manager ];
      homemanager-modules = [
        nur.hmModules.nur
        sops-nix.homeManagerModules.sops
        impermanence.nixosModules.home-manager.impermanence
        (import ./profiles/sops.nix)
      ];
      nixos-modules = [
        sops-nix.nixosModules.sops
        home-manager.nixosModules.home-manager
        nur.nixosModules.nur
        impermanence.nixosModules.impermanence
        xiongchenyu6.nixosModules.bttc
        (_: {
          nixpkgs = {
            system = "x86_64-linux";
            config = {
              allowUnfree = true;
              allowBroken = true;
            };
            overlays = sharedOverlays;
          };
          home-manager.sharedModules = homemanager-modules;
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
        })
      ];
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ inputs.pre-commit-hooks.flakeModule ];
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];

      perSystem =
        { pkgs, ... }:
        {
          packages = {
            iso = self.nixosConfigurations.iso.config.system.build.isoImage;
            bootstrap = self.nixosConfigurations.bootstrap.config.system.build.diskoImagesScript;
          };

          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [
              sops
              ssh-to-age
              editorconfig-checker
              nixfmt-rfc-style
              nil
              statix
              yq-go
              nixos-rebuild
              ruby
            ];
            shellHook = ''
              export NIX_SSHOPTS="-Y -p 2222"
              export PULUMI_CONFIG_PASSPHRASE=""
            '';
          };

          pre-commit = {
            check.enable = true;
            settings = {
              hooks = {
                nixfmt.enable = false;
                statix.enable = true;
                deadnix.enable = true;
                shellcheck.enable = true;
                shfmt.enable = true;
              };
            };
          };
        };

      flake = {
        nixosConfigurations = {
          iso = nixpkgs.lib.nixosSystem { modules = [ ./hosts/nixos/iso.nix ] ++ nixos-modules; };
          bootstrap = nixpkgs.lib.nixosSystem {
            specialArgs = {
              profiles = {
                share = import ./profiles/shares.nix { inherit lib; };
              };
              mylib = import ./lib { inherit lib; };
            };
            modules = [
              disko.nixosModules.disko
              ./hosts/nixos/bootstrap.nix
            ] ++ nixos-modules;
          };

          mail = nixpkgs.lib.nixosSystem {
            specialArgs = {
              profiles = {
                share = import ./profiles/shares.nix { inherit lib; };
              };
              mylib = import ./lib { inherit lib; };
            };
            modules = [
              #xiongchenyu6.nixosModules.oci-arm-host-capacity

              ./hosts/nixos/mail
            ] ++ nixos-modules;
          };
          office = nixpkgs.lib.nixosSystem {
            specialArgs = {
              profiles = {
                share = import ./profiles/shares.nix { inherit lib; };
              };
              mylib = import ./lib { inherit lib; };
            };
            modules = [
              xiongchenyu6.nixosModules.java-tron
              xiongchenyu6.nixosModules.chainlink
              nixos-hardware.nixosModules.lenovo-thinkpad-x1-10th-gen
              ./hosts/nixos/office
            ] ++ nixos-modules;
          };
          office-windows = nixpkgs.lib.nixosSystem {
            specialArgs = {

              profiles = {
                share = import ./profiles/shares.nix { inherit lib; };
              };
              mylib = import ./lib { inherit lib; };
            };
            modules = [
              nixos-wsl.nixosModules.wsl
              vscode-server.nixosModules.default
              ./hosts/windows/office
            ] ++ nixos-modules;
          };
          game = nixpkgs.lib.nixosSystem {
            specialArgs = {
              profiles = {
                share = import ./profiles/shares.nix { inherit lib; };
              };
              mylib = import ./lib { inherit lib; };
            };
            modules = [
              nixos-hardware.nixosModules.lenovo-legion-16ach6h
              ./hosts/nixos/game
            ] ++ nixos-modules;
          };
          digital = nixpkgs.lib.nixosSystem {
            specialArgs = {
              profiles = {
                share = import ./profiles/shares.nix { inherit lib; };
              };
              mylib = import ./lib { inherit lib; };
            };
            modules = [ ./hosts/nixos/digital ] ++ nixos-modules;
          };
          netbird = nixpkgs.lib.nixosSystem {
            specialArgs = {
              profiles = {
                share = import ./profiles/shares.nix { inherit lib; };
              };
              mylib = import ./lib { inherit lib; };
            };
            modules = [
              ./hosts/nixos/netbird
              robot_signal_dashboard.nixosModules.robotSignalDashboard
            ] ++ nixos-modules;
          };

          digitalocean = nixpkgs.lib.nixosSystem {
            specialArgs = {
              profiles = {
                share = import ./profiles/shares.nix { inherit lib; };
              };
              mylib = import ./lib { inherit lib; };
            };

            modules = [ ./hosts/nixos/digitalocean.nix ] ++ nixos-modules;
          };
        };
        darwinConfigurations = {
          XIONGS-MACBOOK-PRO = darwin.lib.darwinSystem { modules = darwin-modules; };
        };
      };
    };
}
