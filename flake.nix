{
  description =
    "Flake to manage my laptop, my nur and my hosts on Tencent Cloud";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    nur.url = "github:nix-community/NUR";

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
      inputs = {
        flake-utils.follows = "flake-utils";
        nixpkgs.follows = "nixpkgs";
      };
    };

    xiongchenyu6 = {
      url = "github:xiongchenyu6/nur-packages";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs = { nixpkgs.follows = "nixpkgs"; };
    };

    flake-utils-plus = {
      url = "github:gytis-ivaskevicius/flake-utils-plus";
      inputs.flake-utils.follows = "flake-utils";
    };

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
      url = "github:divnix/digga";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixlib.follows = "nixpkgs";
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

    grub2-themes = {
      url = "github:vinceliuice/grub2-themes";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprland = {
      url = "github:hyprwm/Hyprland";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprpaper = {
      url = "github:hyprwm/hyprpaper";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    hyprpicker = {
      url = "github:hyprwm/hyprpicker";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    dapptools = {
      url = "github:dapphub/dapptools";
      flake = false;
    };

    foundry = {
      url = "github:xiongchenyu6/foundry.nix";
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

  };

  outputs = { self, nixpkgs, nur, nixos-hardware, emacs, home-manager, devshell
    , pre-commit-hooks, nix-alien, xiongchenyu6, winklink, digga, sops-nix
    , grub2-themes, hyprland, hyprpaper, hyprpicker, dapptools, foundry
    , poetry2nix, ... }@inputs:
    with nixpkgs;
    with lib;
    let
      overlays = map (x: x.overlays.default or x.overlay) [
        emacs
        devshell
        xiongchenyu6
        nix-alien
        sops-nix
        hyprland
        hyprpaper
        hyprpicker
        foundry
        poetry2nix
      ] ++ [
        (_: prev:
          let dapp = import dapptools { inherit (prev) system; };
          in {
            __dontExport = true;
            winklink = winklink.packages."${prev.system}".default;
            hyprland =
              hyprland.packages."${prev.system}".default; # TODO hyprland overlays did not include libdrm in it overlays
            inherit (dapp) hevm dapp ethsign seth;
          })
      ];
    in digga.lib.mkFlake {
      inherit self inputs;

      supportedSystems =
        [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];
      #supportedSystems = allSystems;

      channelsConfig = {
        allowUnfree = true;
        allowBroken = true;
        # allowUnsupportedSystem = true;
      };

      channels = {
        nixpkgs = { imports = [ (digga.lib.importOverlays ./overlays) ]; };
      };

      sharedOverlays = overlays;

      nixos = {
        hostDefaults = {
          channelName = "nixpkgs";
          system = "x86_64-linux";

          modules = [
            sops-nix.nixosModules.sops
            digga.nixosModules.nixConfig
            home-manager.nixosModules.home-manager
            nur.nixosModules.nur

            # xiongchenyu6.nixosModules.bttc
          ];
        };
        imports = [ (digga.lib.importHosts ./hosts/nixos) ];
        hosts = {
          arm = { system = "aarch64-linux"; };
          mail = {
            modules = [ xiongchenyu6.nixosModules.oci-arm-host-capacity ];
          };
          office = {
            modules = [
              grub2-themes.nixosModule
              hyprland.nixosModules.default
              nixos-hardware.nixosModules.lenovo-thinkpad-x1-9th-gen
              nixos-hardware.nixosModules.common-gpu-intel
            ];
          };
        };

        importables = rec {
          profiles = digga.lib.rakeLeaves ./profiles // {
            users = digga.lib.rakeLeaves ./users;
            share = import ./profiles/shares.nix;
          };
          suites = with profiles; rec {
            base = [ core.nixos sops ];
            common-components = builtins.attrValues profiles.common-components;
            common-apps = builtins.attrValues profiles.common-apps;
            client-components = builtins.attrValues profiles.client-components;
            server-components = builtins.attrValues profiles.server-components;
            client-base = base ++ common-apps ++ common-components
              ++ client-components ++ [ bird2 auto-login.getty ];
            server-base = base ++ common-apps ++ common-components
              ++ server-components
              ++ [ server-apps.log.promtail server-apps.admin.sssd ];
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

        imports = [ (digga.lib.importHosts ./hosts/darwin) ];
        hosts = { XIONGs-MacBook-Pro = { system = "x86_64-darwin"; }; };
        importables = rec {
          profiles = digga.lib.rakeLeaves ./profiles // {
            users = digga.lib.rakeLeaves ./users;
            share = import ./profiles/shares.nix { };
          };
          suites = with profiles; {
            base = [ core.darwin ];
            full = [ core.darwin client-pkgs.darwin ];
          };
        };
      };

      home = {
        modules = [ nur.hmModules.nur hyprland.homeManagerModules.default ];

        importables = rec {
          profiles = digga.lib.rakeLeaves ./users/profiles // {
            share = import ./profiles/shares.nix;
          };
          suites = with profiles; {
            nix-remote-build = [ use-remote-builder ];
            cli = [ cli.common cli.shell.zsh ];
            linux-gui = [
              gui.nixos
              gui.window-manager.hyprland
              cli.common
              gui.mpd
              cli.shell.zsh
            ];
            mac-gui = [ gui.darwin cli.common cli.shell.zsh ];
          };
        };
        users = {
          root = { suites, ... }: { imports = suites.nix-remote-build; };
          freeman-cli = { suites, ... }: { imports = suites.cli; };
          freeman-gui = { suites, ... }: { imports = suites.linux-gui; };
          xiongchenyu = { suites, ... }: { imports = suites.mac-gui; };
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
                deadnix.enable = true;
                shellcheck.enable = true;
                shfmt.enable = true;
              };
            };
        };
      };

      deploy = {
        sshOpts = [ "-Y" "-p" "2222" ];
        autoRollback = false;
        magicRollback = false;
        fastConnection = true;
        nodes = digga.lib.mkDeployNodes self.nixosConfigurations {
          mail = { profiles = { system = { sshUser = "root"; }; }; };
          arm = { profiles = { system = { sshUser = "root"; }; }; };
        };
      };
    };
}
