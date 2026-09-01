{
  # nixConfig.extra-experimental-features = "nix-command flakes";

  description = "Flake to manage my laptop, my nur and my hosts on Tencent Cloud";

  inputs = {
    # Core inputs
    systems.url = "github:nix-systems/default";
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-24.11";
    #nixpkgs-master.url = "github:NixOS/nixpkgs/master";
    nixos-hardware = {
      url = "github:NixOS/nixos-hardware/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nur = {
      url = "github:nix-community/NUR";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
      };
    };

    # Flake utilities
    flake-compat.url = "github:edolstra/flake-compat";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.systems.follows = "systems";
    };

    # System management
    impermanence.url = "github:nix-community/impermanence/home-manager-v1";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    disko = {
      url = "github:nix-community/disko";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    srvos = {
      url = "github:nix-community/srvos";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    NixVirt = {
      url = "github:AshleyYakeley/NixVirt/v0.6.0";
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
    # Development tools
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-compat.follows = "flake-compat";
      };
    };
    nix-alien = {
      url = "github:thiagokokada/nix-alien";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-compat.follows = "flake-compat";
      };
    };
    # Personal and project packages
    xiongchenyu6 = {
      url = "github:xiongchenyu6/nur-packages";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
      };
    };
    vast-cli = {
      url = "github:dialohq/vast-cli.nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
      };
    };
    nix-topology = {
      url = "github:oddlama/nix-topology";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
      };
    };
    ez-configs = {
      url = "github:ehllie/ez-configs";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
      };
    };
    #claude
    claude-desktop = {
      url = "github:k3d3/claude-desktop-linux-flake";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
    llm-agents = {
      url = "github:numtide/llm-agents.nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        systems.follows = "systems";
        flake-parts.follows = "flake-parts";
      };
    };

    rime-frost = {
      url = "github:gaboolic/rime-frost";
      flake = false;
    };

    # 万象 LTS 语法模型(octagram),整句/长句准确率优于自带的 zh-moqi
    rime-wanxiang-gram = {
      url = "file+https://github.com/amzxyz/RIME-LMDG/releases/download/LTS/wanxiang-lts-zh-hans.gram";
      flake = false;
    };

    hermes-agent = {
      url = "github:NousResearch/hermes-agent";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
      };
    };

    lanzaboote = {
      url = "github:nix-community/lanzaboote";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        pre-commit.follows = "pre-commit-hooks";
      };
    };

    niri = {
      url = "github:sodiboo/niri-flake";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixpkgs-stable.follows = "nixpkgs-stable";
      };
    };

    noctalia = {
      url = "github:noctalia-dev/noctalia";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    talon-nix = {
      url = "github:nix-community/talon-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # LazyNixOS - lazy evaluation and deployment
    lazynixos = {
      url = "github:xiongchenyu6/lazynixos";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-parts,
      ...
    }@inputs:
    with nixpkgs;
    with lib;
    let
      mylib = import ./lib { inherit lib; };
      shares = import ./shared-modules/shares.nix { inherit lib; };
    in
    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [
        inputs.pre-commit-hooks.flakeModule
        inputs.nix-topology.flakeModule
        inputs.ez-configs.flakeModule
      ];
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];

      ezConfigs = {
        globalArgs = { inherit inputs shares mylib; };
        root = ./.;
        darwin.hosts = {
          office-mac = {
            userHomeModules = [
              "freeman.xiong"
            ];
          };
        };
        nixos.hosts =
          let
            userHomeModules = [
              "root"
              "freeman.xiong"
            ];
            hostConfig = { inherit userHomeModules; };
          in
          {
            game = hostConfig;
            office = hostConfig;
            autolife-robot-260 = hostConfig;

            tcloud = hostConfig;
            oracle-arm-001 = hostConfig;
            oracle-arm-002 = hostConfig;
            oracle-amd-001 = hostConfig;
            oracle-amd-002 = hostConfig;
          };

        # lubancat is an aarch64 SBC on Ubuntu — RK3588 has no mainline device
        # tree, so it cannot run NixOS. home-manager still manages the shell
        # there, standalone.
        #
        # standalone suppresses this user's ${user}@${host} outputs, which is
        # why only `cat` uses it: turning it on for freeman.xiong would drop
        # every existing per-host home configuration.
        home.users.cat = {
          importDefault = false;
          standalone = {
            enable = true;
            pkgs = import inputs.nixpkgs {
              system = "aarch64-linux";
              config.allowUnfree = true;
            };
          };
        };
      };

      perSystem =
        { pkgs, system, ... }:
        {
          packages = {
            iso = self.nixosConfigurations.iso.config.system.build.isoImage;
          };

          devShells.default = pkgs.mkShell {
            buildInputs =
              with pkgs;
              [
                sops
                ssh-to-age
                editorconfig-checker
                nixfmt
                nixd
                statix
                nixos-anywhere
                yaml-language-server
                gnupg
                yq-go
                nixos-rebuild
              ]
              ++ lib.optionals pkgs.stdenv.hostPlatform.isDarwin [
                # Darwin-specific build tools
                inputs.darwin.packages.${system}.darwin-rebuild
              ];
          };
          #export NIX_SSHOPTS="-Y -p 2222"
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
    };
}
