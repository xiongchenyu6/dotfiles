{
  # nixConfig.extra-experimental-features = "nix-command flakes";

  description = "Flake to manage my laptop, my nur and my hosts on Tencent Cloud";

  inputs = {
    systems.url = "github:nix-systems/default";

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-24.11";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    nur.url = "github:nix-community/NUR";

    flake-compat.url = "github:edolstra/flake-compat";

    flake-parts.url = "github:hercules-ci/flake-parts";

    impermanence.url = "github:nix-community/impermanence";

    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs = {
        systems.follows = "systems";
      };
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };

    nix-vscode-extensions = {
      url = "github:nix-community/nix-vscode-extensions";
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
        flake-compat.follows = "flake-compat";
      };
    };

    xiongchenyu6 = {
      url = "github:xiongchenyu6/nur-packages";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
      };
    };

    nix-alien = {
      url = "github:thiagokokada/nix-alien";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        flake-compat.follows = "flake-compat";
      };
    };

    sops-nix = {
      #url = "github:Mic92/sops-nix";
      url = "github:Mic92/sops-nix/a4c33bfecb93458d90f9eb26f1cf695b47285243";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixpkgs-stable";
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
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
        poetry2nix.follows = "poetry2nix";
        flake-utils.follows = "flake-utils";
        systems.follows = "systems";
        flake-compat.follows = "flake-compat";
      };
    };

    robot_signal_dashboard = {
      url = "git+ssh://git@github.com/AutoLifeRobot/robot_signal_dashboard.git";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
    };

    autolife_www = {
      url = "git+ssh://git@github.com/AutoLifeRobot/www.git";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-parts.follows = "flake-parts";
    };

    srvos = {
      url = "github:nix-community/srvos";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-compat.follows = "flake-compat";
      };
    };

    nix-topology = {
      url = "github:oddlama/nix-topology";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    nixos-wsl = {
      url = "github:nix-community/NixOS-WSL/main";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-compat.follows = "flake-compat";
      };
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
        systems.follows = "systems";
      };
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
      autolife_www,
      srvos,
      nix-topology,
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
        nix-topology
      ];

      specialArgs = {
        profiles = {
          share = import ./profiles/shares.nix { inherit lib; };
        };
        mylib = import ./lib { inherit lib; };
      };

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
      sharedModules = [
        #nur.modules.home-manager
        sops-nix.homeManagerModules.sops
        impermanence.nixosModules.home-manager.impermanence
        (import ./profiles/sops.nix)
      ];
      nixos-modules = [
        sops-nix.nixosModules.sops
        home-manager.nixosModules.home-manager
        nur.modules.nixos.default
        impermanence.nixosModules.impermanence
        nix-topology.nixosModules.default
        (_: {
          nixpkgs = {
            system = "x86_64-linux";
            config = {
              allowUnfree = true;
              allowBroken = true;
              android_sdk.accept_license = true;
            };
            overlays = sharedOverlays;
          };
          home-manager = {
            inherit sharedModules;
            useGlobalPkgs = true;
            useUserPackages = true;
          };
        })
      ];
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.pre-commit-hooks.flakeModule
        inputs.nix-topology.flakeModule
      ];
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
          };

          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [
              sops
              ssh-to-age
              editorconfig-checker
              nixfmt-rfc-style
              nixd
              statix
              yq-go
              nixos-rebuild-ng
              nixos-facter
              nixos-anywhere
            ];
            shellHook = ''
              export $(sops -d ./secrets/common.env | xargs)
            '';
          };
          #export NIX_SSHOPTS="-Y -p 2222"
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
          topology = {
            nixosConfigurations = builtins.removeAttrs self.nixosConfigurations [
              "do"
              "generic-nixos-facter"
            ];
            modules = [
              (
                { config, ... }:
                let
                  inherit (config.lib.topology) mkInternet mkRouter mkConnection;
                in
                {
                  networks.home = {
                    name = "Home";
                    cidrv4 = "192.168.178.0/24";
                  };
                }
              )
            ];
          };
        };

      flake = {

        nixosConfigurations = {
          iso = nixpkgs.lib.nixosSystem { modules = [ ./hosts/nixos/iso.nix ] ++ nixos-modules; };

          # office-lenovo = nixpkgs.lib.nixosSystem {
          #   inherit specialArgs;
          #   modules = [
          #     nixos-hardware.nixosModules.lenovo-thinkpad-x1-10th-gen
          #     ./hosts/nixos/office
          #     {
          #       topology.networks.home = {
          #         name = "Network Made by Lenovo in Office";
          #         cidrv4 = "192.168.178.1/24";
          #       };
          #     }
          #   ] ++ nixos-modules;
          # };
          office = nixpkgs.lib.nixosSystem {
            inherit specialArgs;
            modules = [
              #srvos.nixosModules.desktop
              nixos-hardware.nixosModules.dell-latitude-5520
              ./hosts/nixos/office
              {
                topology.self.interfaces.home = {

                  type = "wireguard"; # changes the icon
                  addresses = [ "192.168.178.2/24" ];
                };

              }
            ] ++ nixos-modules;
          };

          # office-windows = nixpkgs.lib.nixosSystem {
          #   inherit specialArgs;
          #   modules = [
          #     nixos-wsl.nixosModules.wsl
          #     vscode-server.nixosModules.default
          #     ./hosts/windows/office
          #     {
          #       topology.interfaces.home = {
          #         name = "Network Made in Office Windows";
          #         cidrv4 = "192.168.178.3/24";
          #       };

          #     }
          #   ] ++ nixos-modules;
          # };

          game-office = nixpkgs.lib.nixosSystem {
            inherit specialArgs;
            modules = [
              #srvos.nixosModules.desktop
              nixos-hardware.nixosModules.lenovo-legion-16ach6h
              ./hosts/nixos/game-office
              {
                topology.self.interfaces.home = {

                  addresses = [ "192.168.178.4/24" ];
                };

              }
            ] ++ nixos-modules;
          };

          game = nixpkgs.lib.nixosSystem {
            inherit specialArgs;
            modules = [
              nixos-hardware.nixosModules.lenovo-legion-16ach6h
              srvos.nixosModules.mixins-mdns
              vscode-server.nixosModules.default
              srvos.nixosModules.mixins-trusted-nix-caches
              srvos.nixosModules.mixins-nix-experimental
              srvos.nixosModules.mixins-tracing
              ./hosts/nixos/game
              {
                topology.self.interfaces.home = {

                  addresses = [ "192.168.178.4/24" ];
                };

              }

            ] ++ nixos-modules;
          };

          mail = nixpkgs.lib.nixosSystem {
            inherit specialArgs;
            modules = [
              srvos.nixosModules.server
              srvos.nixosModules.mixins-nginx
              ./hosts/nixos/mail
              {
                topology.self.interfaces.home = {

                  addresses = [ "192.168.178.5/24" ];
                };

              }

            ] ++ nixos-modules;
          };

          digital = nixpkgs.lib.nixosSystem {
            inherit specialArgs;
            modules = [
              #srvos.nixosModules.hardware-digitalocean-droplet
              srvos.nixosModules.server
              srvos.nixosModules.mixins-nginx
              ./hosts/nixos/digital
              {
                topology.self.interfaces.home = {
                  addresses = [ "192.168.178.6/24" ];
                };
              }

            ] ++ nixos-modules;
          };

          netbird = nixpkgs.lib.nixosSystem {
            inherit specialArgs;
            modules = [
              srvos.nixosModules.server
              srvos.nixosModules.hardware-amazon
              srvos.nixosModules.mixins-nginx
              ./hosts/nixos/netbird
              robot_signal_dashboard.nixosModules.robotSignalDashboard
              {
                topology.self.interfaces.home = {

                  addresses = [ "192.168.178.7/24" ];
                };
              }
            ] ++ nixos-modules;
          };

          digitalocean = nixpkgs.lib.nixosSystem {
            inherit specialArgs;
            modules = [
              ./hosts/nixos/digitalocean.nix
              {
                topology.self.interfaces.home = {

                  addresses = [ "192.168.178.8/24" ];
                };
              }
            ] ++ nixos-modules;
          };

          huawei-bj-001 = nixpkgs.lib.nixosSystem {
            inherit specialArgs;
            modules = [
              ./hosts/nixos/huawei-bj-001
              srvos.nixosModules.server
              srvos.nixosModules.mixins-nginx
              robot_signal_dashboard.nixosModules.robotSignalDashboard
            ] ++ nixos-modules;
          };

          huoshan-bj-001 = nixpkgs.lib.nixosSystem {
            inherit specialArgs;
            modules = [
              ./hosts/nixos/huoshan-bj-001
              srvos.nixosModules.server
              srvos.nixosModules.mixins-nginx
              robot_signal_dashboard.nixosModules.robotSignalDashboard
            ] ++ nixos-modules;
          };

          generic-nixos = nixpkgs.lib.nixosSystem {
            inherit specialArgs;
            system = "x86_64-linux";
            modules = [
              ./hosts/nixos/nixos-anywhere
              disko.nixosModules.disko
              srvos.nixosModules.server
              ./hardware-configuration.nix
            ];
          };
          do = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            inherit specialArgs;
            modules = [
              srvos.nixosModules.server
              srvos.nixosModules.mixins-nginx
              disko.nixosModules.disko
              ./hosts/nixos/nixos-anywhere
              { disko.devices.disk.disk1.device = nixpkgs.lib.mkForce "/dev/vda"; }
              {
                # do not use DHCP, as DigitalOcean provisions IPs using cloud-init
                networking.useDHCP = nixpkgs.lib.mkForce false;

                services.cloud-init = {
                  enable = true;
                  network.enable = true;
                  settings.datasource_list = [ "DigitalOcean" ];
                  settings.datasource.DigitalOcean = { };
                };
              }
            ];
          };
        };
        darwinConfigurations = {
          XIONGS-MACBOOK-PRO = darwin.lib.darwinSystem { modules = darwin-modules; };
        };
      };
    };
}
