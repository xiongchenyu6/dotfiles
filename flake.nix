{
  # nixConfig.extra-experimental-features = "nix-command flakes";

  description =
    "Flake to manage my laptop, my nur and my hosts on Tencent Cloud";

  inputs = {

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-23.05";

    nixpkgs-master.url = "github:NixOS/nixpkgs/master";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    nur.url = "github:nix-community/NUR";

    flake-utils.url = "github:numtide/flake-utils";

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs = { nixpkgs.follows = "nixpkgs"; };
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

    devenv = {
      url = "github:cachix/devenv";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        pre-commit-hooks.follows = "pre-commit-hooks";
      };
    };
    nix2container = {
      url = "github:nlewo/nix2container";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    # nix-ros-overlay = { url = "github:lopsided98/nix-ros-overlay"; };
    mk-shell-bin.url = "github:rrbutani/nix-mk-shell-bin";

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
        flake-utils.follows = "flake-utils";
      };
    };

    impermanence.url = "github:nix-community/impermanence";

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
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
    vscode-server.url = "github:nix-community/nixos-vscode-server";

    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, nixpkgs-stable, impermanence, nur, nixos-hardware
    , home-manager, devenv, flake-parts, pre-commit-hooks, nix-alien
    , xiongchenyu6, sops-nix, foundry, poetry2nix, nix-vscode-extensions
    , nixos-wsl, vscode-server, nixpkgs-master, disko, ... }@inputs:
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
          lib = prev.lib.extend
            (_lfinal: _lprev: { mine = import ./lib { inherit lib; }; });
          # gnupg240 = nixpkgs-stable.legacyPackages.x86_64-linux.gnupg;
          # telegram-desktop =
          #   nixpkgs-stable.legacyPackages.x86_64-linux.telegram-desktop;
          #     waybar = nixpkgs-master.legacyPackages.x86_64-linux.waybar;
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

    in flake-parts.lib.mkFlake { inherit inputs; } {
      imports =
        [ inputs.devenv.flakeModule inputs.pre-commit-hooks.flakeModule ];
      systems =
        [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];

      perSystem = { pkgs, ... }: {
        packages = {
          iso = self.nixosConfigurations.iso.config.system.build.isoImage;
          bootstrap =
            self.nixosConfigurations.bootstrap.config.system.build.diskoImagesScript;
        };

        apps = let
          type = "app";
          getip = ''
            usage() {
              echo "Usage: $0 [-p] [-u user] hostname" >&2
              echo "  -p        Get public IP address" >&2
              echo "  -u user   SSH username (default: root)" >&2
              exit 1
            }

            public=false
            user="root"

            while getopts "pu:" opt; do
              case ''${opt} in
                p)
                  public=true
                  ;;
                u)
                  user=$OPTARG
                  ;;
                \?)
                  echo "Invalid option: -$OPTARG" >&2
                  usage
                  ;;
              esac
            done

            shift $((OPTIND -1))

            if [ $# -ne 1 ]; then
              usage
            fi

            host=$1
            json_file=pulumi.json

            if [ "$public" = true ]; then
              ip_type="''${host}-public-ip"
            else
              ip_type="''${host}-private-ip"
            fi

            # Use jq to extract the IP address from the JSON file
            echo ''${ip_type}
            ip_address=$(jq -r .\"''${ip_type}\" $json_file)

            if [ -z "$ip_address" ]; then
              echo "Error: Could not find IP address for host '$host' in file '$json_file'"
              exit 1
            fi
          '';
        in {
          ssh-to = {
            inherit type;
            program = builtins.toString (pkgs.writeShellScript "ssh-to" ''
              ${getip}
              zssh $NIX_SSHOPTS ''${user}@''${ip_address}
            '');
          };

          deploy = {
            inherit type;
            program = builtins.toString (pkgs.writeShellScript "deploy" ''
              ${getip}
              nixos-rebuild --target-host ''${user}@''${ip_address} switch --use-remote-sudo --flake .\#''${host} --verbose
            '');
          };

          dry-build = {
            inherit type;
            program = builtins.toString (pkgs.writeShellScript "dry-build" ''
              ${getip}
              nixos-rebuild --target-host ''${user}@''${ip_address} dry-build --use-remote-sudo --flake .\#''${host} --verbose
            '');
          };

          updateHosts = {
            inherit type;
            program = builtins.toString (pkgs.writeShellScript "updateHosts" ''
              set -x
              hosts=$(jq -r 'keys[] | select(test("-private-ip$|-public-ip$"))' pulumi.json | sed -e 's/-private-ip//' -e 's/-public-ip//' | sort -u)

              # Loop through each host
              for host in $hosts
              do
                # Check if folder exists, if not create it
                if [ ! -d "./hosts/$host" ]
                then
                  mkdir "./hosts/$host"
                  ip_type="''${host}-private-ip"
                  ip_address=$(jq -r .\"''${ip_type}\" pulumi.json)
                  age_key=$(ssh-keyscan -t ed25519 $ip_address | ssh-to-age)

                  # Append the value to the array
                  yq e ".keys += [\"$age_key\"] | (.keys[-1] anchor=\"$host\")" -i ./.sops.yaml

                  KEY=.creation_rules[0].key_groups[0].age
                  VALUE="*$host"

                  yq e "$KEY += [\"\"] | ($KEY[-1] alias=\"$host\")" -i ./.sops.yaml
                  echo "Folder for host $host created"
                fi
              done
            '');
          };
        };

        devenv.shells.default = {
          name = "my-project";

          # This is your devenv configuration
          env = {
            NIX_SSHOPTS = "-Y -p 2222 -i ~/.ssh/id_ed25519";
            PULUMI_CONFIG_PASSPHRASE = "";
          };
          packages = with pkgs; [
            sops
            ssh-to-age
            editorconfig-checker
            mdbook
            nixfmt
            statix
            dasel
            yq-go
            nixos-rebuild
            pulumi-bin
            ruby_3_2
            nil
          ];
          languages = {
            go = { enable = true; };
            nix = { enable = true; };
          };
        };
        pre-commit = {
          check.enable = true;
          settings = {
            hooks = {
              nixfmt.enable = true;
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
          iso =
            nixpkgs.lib.nixosSystem { modules = [ ./hosts/nixos/iso.nix ]; };
          bootstrap = nixpkgs.lib.nixosSystem {
            specialArgs = {
              profiles = {
                share = import ./profiles/shares.nix { inherit lib; };
              };
              mylib = import ./lib { inherit lib; };
            };
            modules = [
              impermanence.nixosModules.impermanence
              disko.nixosModules.disko
              ./hosts/nixos/bootstrap.nix
            ];
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
              nixos-hardware.nixosModules.lenovo-legion-16ach6h-hybrid
              ./hosts/nixos/game
            ] ++ nixos-modules;
          };
        };
        darwinConfigurations = {
          XIONGS-MACBOOK-PRO =
            darwin.lib.darwinSystem { modules = darwin-modules; };
        };
      };
    };
}

