{
  description = "Flake to manage my laptop, my nur and my hosts on Tencent Cloud";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    flake-utils.url = "github:numtide/flake-utils";

    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";
      };
    };

    darwin = {
      url = "github:lnl7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    devshell = {
      url = "github:numtide/devshell";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    emacs = {
      url = "github:nix-community/emacs-overlay";
      inputs = {nixpkgs.follows = "nixpkgs";};
    };

    xddxdd = {
      url = "github:xddxdd/nur-packages";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    xiongchenyu6 = {
      url = "github:xiongchenyu6/nur-packages";
      #url = "/home/freeman/private/nur-packages";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs = {nixpkgs.follows = "nixpkgs";};
    };

    flake-utils-plus = {
      url = "github:gytis-ivaskevicius/flake-utils-plus";
      inputs.flake-utils.follows = "flake-utils";
    };

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = {flake-utils.follows = "flake-utils";};
    };

    nix-alien = {
      url = "github:thiagokokada/nix-alien";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    winklink = {
      url = "github:xiongchenyu6/winklink";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        devshell.follows = "devshell";
      };
    };

    deploy-rs = {
      url = "github:serokell/deploy-rs";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";
        flake-compat.follows = "flake-compat";
      };
    };

    digga = {
      url = "github:divnix/digga/home-manager-22.11";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixlib.follows = "nixpkgs";
        latest.follows = "nixpkgs";
        nixpkgs-unstable.follows = "nixpkgs";
        darwin.follows = "darwin";
        devshell.follows = "devshell";
        home-manager.follows = "home-manager";
        deploy.follows = "deploy-rs";
        flake-compat.follows = "flake-compat";
      };
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      # optional, not necessary for the module
      inputs.nixpkgs.follows = "nixpkgs";
    };

    grub2-themes.url = "github:vinceliuice/grub2-themes";
    grub2-themes.inputs.nixpkgs.follows = "nixpkgs";

    hyprland.url = "github:vaxerski/Hyprland";
    hyprland.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    nixos-hardware,
    emacs,
    xddxdd,
    flake-utils,
    flake-utils-plus,
    home-manager,
    devshell,
    pre-commit-hooks,
    nix-alien,
    xiongchenyu6,
    winklink,
    digga,
    sops-nix,
    grub2-themes,
    hyprland,
    ...
  } @ inputs:
    with nixpkgs;
    with lib;
    with flake-utils.lib;
    with flake-utils-plus.lib; let
      overlays =
        map (x: x.overlay or x.overlays.default) [
          emacs
          devshell
          xddxdd
          xiongchenyu6
          nix-alien
          sops-nix
          hyprland
        ]
        ++ [
          (_: prev: {
            __dontExport = true;
            winklink = winklink.packages."${prev.system}".default;
          })
        ];
    in
      digga.lib.mkFlake {
        inherit self inputs;

        supportedSystems = ["x86_64-linux" "aarch64-darwin" "x86_64-darwin"];
        #supportedSystems = allSystems;

        channelsConfig = {
          allowUnfree = true;
          allowBroken = true;
          # allowUnsupportedSystem = true;
        };

        channels = {
          nixpkgs = {imports = [(digga.lib.importOverlays ./overlays)];};
        };

        sharedOverlays = overlays;

        nixos = {
          hostDefaults = {
            channelName = "nixpkgs";
            modules = [
              sops-nix.nixosModules.sops
              nixos-hardware.nixosModules.lenovo-thinkpad-x1-9th-gen
              nixos-hardware.nixosModules.common-gpu-intel
              digga.darwinModules.nixConfig
              home-manager.nixosModules.home-manager
              xiongchenyu6.nixosModules.bttc
              grub2-themes.nixosModule
              hyprland.nixosModules.default
            ];
          };
          hosts = {
            mail = {
              modules = [xiongchenyu6.nixosModules.oci-arm-host-capacity];
            };
          };

          imports = [(digga.lib.importHosts ./hosts/nixos)];
          importables = rec {
            profiles =
              digga.lib.rakeLeaves ./profiles
              // {
                users = digga.lib.rakeLeaves ./users;
                share = import ./profiles/shares.nix;
              };
            suites = with profiles; rec {
              base = [core.nixos sops];
              common-components = builtins.attrValues profiles.common-components;
              common-apps = builtins.attrValues profiles.common-apps;
              client-components = builtins.attrValues profiles.client-components;
              server-apps = builtins.attrValues profiles.server-apps;
              server-components = builtins.attrValues profiles.server-components;
              client-base =
                base
                ++ common-apps
                ++ common-components
                ++ [profiles.bird2 profiles.auto-login.getty]
                ++ client-components;
              server-base =
                base
                ++ common-apps
                ++ common-components
                ++ server-apps
                ++ server-components;
            };
          };
        };

        darwin = {
          hostDefaults = {
            system = "aarch64-darwin";
            channelName = "nixpkgs";
            modules = [
              digga.darwinModules.nixConfig
              home-manager.darwinModules.home-manager
            ];
          };

          imports = [(digga.lib.importHosts ./hosts/darwin)];
          hosts = {XIONGs-MacBook-Pro = {system = "x86_64-darwin";};};
          importables = rec {
            profiles =
              digga.lib.rakeLeaves ./profiles
              // {
                users = digga.lib.rakeLeaves ./users;
                share = import ./profiles/shares.nix {};
              };
            suites = with profiles; {
              base = [core.darwin];
              full = [core.darwin client-pkgs.darwin];
            };
          };
        };

        home = {
          modules = [hyprland.homeManagerModules.default];

          importables = rec {
            profiles =
              digga.lib.rakeLeaves ./users/profiles
              // {
                share = import ./profiles/shares.nix;
              };
            suites = with profiles; {
              cli = [cli];
              linux-gui = [gui.nixos gui.window-manager.hyprland cli];
              mac-gui = [gui.darwin cli];
            };
          };
          users = {
            freeman-cli = {suites, ...}: {imports = suites.cli;};
            freeman-gui = {suites, ...}: {imports = suites.linux-gui;};
            xiongchenyu = {suites, ...}: {imports = suites.mac-gui;};
          };
        };

        devshell = ./shell;

        outputsBuilder = channels: {
          checks = {
            pre-commit-check = pre-commit-hooks.lib."${channels.nixpkgs.system}".run {
              src = ./.;
              hooks = {
                alejandra.enable = true;
                statix.enable = true;
                nix-linter.enable = true;
              };
            };
          };
        };

        deploy = {
          sshOpts = ["-X" "-p" "2222"];
          autoRollback = false;
          magicRollback = false;
          fastConnection = true;
          nodes = digga.lib.mkDeployNodes self.nixosConfigurations {
            mail = {profiles = {system = {sshUser = "root";};};};
          };
        };
      };
}
