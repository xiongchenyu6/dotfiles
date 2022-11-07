{
  description =
    "Flake to manage my laptop, my nur and my hosts on Tencent Cloud";

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

    nixpkgs-darwin-stable.url = "github:NixOS/nixpkgs/nixpkgs-22.05-darwin";

    devshell = {
      url = "github:numtide/devshell";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    emacs = {
      url = "github:nix-community/emacs-overlay";
      inputs = { nixpkgs.follows = "nixpkgs"; };
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

    agenix = {
      url = "github:ryantm/agenix";
      inputs = { nixpkgs.follows = "nixpkgs"; };
    };

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs = { nixpkgs.follows = "nixpkgs"; };
    };

    flake-utils-plus = {
      url = "github:gytis-ivaskevicius/flake-utils-plus";
      inputs.flake-utils.follows = "flake-utils";
    };

    nixops = {
      url = "github:NixOS/nixops";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        utils.follows = "flake-utils";
      };
    };

    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs = { flake-utils.follows = "flake-utils"; };
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
      url = "github:divnix/digga";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixlib.follows = "nixpkgs";
        home-manager.follows = "home-manager";
        deploy.follows = "deploy-rs";
      };
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      # optional, not necessary for the module
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Common Grub2 themes
    grub2-themes.url = "github:vinceliuice/grub2-themes";
    grub2-themes.inputs.nixpkgs.follows = "nixpkgs";
    grub2-themes-png.url = "github:AnotherGroupChat/grub2-themes-png";
    grub2-themes-png.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, nixos-hardware, emacs, xddxdd, flake-utils
    , flake-utils-plus, home-manager, agenix, nixos-generators, devshell, nixops
    , gradle2nix, pre-commit-hooks, nix-alien, xiongchenyu6, winklink, deploy-rs
    , digga, sops-nix, ... }@inputs:
    with nixpkgs;
    with lib;
    with flake-utils.lib;
    with flake-utils-plus.lib;
    let
      overlays = map (x: x.overlay or x.overlays.default) [
        agenix
        emacs
        devshell
        xddxdd
        xiongchenyu6
        nix-alien
        sops-nix
      ] ++ [
        (final: prev: {
          __dontExport = true;
          winklink = winklink.packages."${prev.system}".default;
        })
      ];
      pkgsFor = system: import nixpkgs { inherit system overlays; };
    in digga.lib.mkFlake {
      inherit self inputs;

      supportedSystems = [ "x86_64-linux" "aarch64-darwin" "x86_64-darwin" ];
      #supportedSystems = allSystems;

      channelsConfig = {
        allowUnfree = true;
        allowBroken = true;
      };

      channels = {
        nixpkgs = { imports = [ (digga.lib.importOverlays ./overlays) ]; };
        nixpkgs-darwin-stable = {
          overlays = [
            # TODO: restructure overlays directory for per-channel overrides
            # `importOverlays` will import everything under the path given
            (channels: final: prev:
              {
                inherit (channels.nixpkgs) mas;
              } // prev.lib.optionalAttrs true { })
          ];
        };
      };

      sharedOverlays = overlays;

      nixos = {
        hostDefaults = {
          channelName = "nixpkgs";
          modules = [
            sops-nix.nixosModules.sops
            nixos-hardware.nixosModules.lenovo-thinkpad-x1-9th-gen
            nixos-hardware.nixosModules.common-gpu-intel
            agenix.nixosModule
            digga.darwinModules.nixConfig
            home-manager.nixosModules.home-manager
            xiongchenyu6.nixosModules.bttc
          ];
        };
        hosts = {
          mail = {
            modules = [ xiongchenyu6.nixosModules.oci-arm-host-capacity ];
          };
        };

        imports = [ (digga.lib.importHosts ./hosts/nixos) ];
        importables = rec {
          profiles = digga.lib.rakeLeaves ./profiles // {
            users = digga.lib.rakeLeaves ./users;
          };
          suites = with profiles; rec {
            base = [ core.nixos sops ];
            common-components = builtins.attrValues profiles.common-components;
            common-apps = builtins.attrValues profiles.common-apps;
            client-components = builtins.attrValues profiles.client-components;
            client-apps = builtins.attrValues profiles.client-apps;
            server-apps = builtins.attrValues profiles.server-apps;
            server-components = builtins.attrValues profiles.server-components;
            client-base = base ++ common-apps ++ common-components
              ++ client-apps ++ client-components;
            server-base = base ++ common-apps ++ common-components
              ++ server-apps ++ server-components;
          };
        };
      };

      darwin = {
        hostDefaults = {
          system = "x86_64-darwin";
          channelName = "nixpkgs-darwin-stable";
          modules = [
            digga.darwinModules.nixConfig
            home-manager.darwinModules.home-manager
            agenix.nixosModules.age
          ];
        };

        imports = [ (digga.lib.importHosts ./hosts/darwin) ];
        hosts = { };
        importables = rec {
          profiles = digga.lib.rakeLeaves ./profiles // {
            users = digga.lib.rakeLeaves ./users;
          };
          suites = with profiles; rec {
            base = [ core.darwin client-apps.darwin ];
          };
        };
      };

      home = {
        importables = rec {
          profiles = digga.lib.rakeLeaves ./users/profiles;
          suites = with profiles; rec { base = [ cli ]; };
        };
        users = {
          freeman-cli = { suites, config, profiles, ... }: {
            imports = suites.base;
          };
          freeman-gui = { suites, config, profiles, ... }: {
            imports = suites.base ++ [ profiles.gui ];
          };
        };
      };

      devshell = ./shell;

      outputsBuilder = channels: {
        checks = {
          pre-commit-check =
            pre-commit-hooks.lib."${channels.nixpkgs.system}".run {
              src = ./.;
              hooks = {
                nixfmt.enable = true;
                statix.enable = true;
                nix-linter.enable = true;
              };
            };
        };
      };

      deploy = {
        sshOpts = [ "-X" "-p" "2222" ];
        # autoRollback = false;
        # magicRollback = false;
        fastConnection = true;
        nodes = digga.lib.mkDeployNodes self.nixosConfigurations {
          mail = { profiles = { system = { sshUser = "root"; }; }; };
        };
      };
    };
}
