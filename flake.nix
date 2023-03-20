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

    impermanence.url = "github:nix-community/impermanence";

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
      url = "github:wink-link/winklink/feature/update-java-and-dependencies";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        devshell.follows = "devshell";
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

    nil = {
      url = "github:oxalica/nil";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        rust-overlay.url = "github:oxalica/rust-overlay";
        rust-overlay.inputs.flake-utils.follows = "flake-utils";
        rust-overlay.inputs.nixpkgs.follows = "nixpkgs";
      };
    };

  };

  outputs = { self, nixpkgs, impermanence, nur, nixos-hardware, emacs
    , home-manager, devshell, pre-commit-hooks, nix-alien, xiongchenyu6
    , winklink, digga, sops-nix, grub2-themes, hyprland, hyprpaper, hyprpicker
    , foundry, nil, poetry2nix, ... }@inputs:
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
        nil
        poetry2nix
      ] ++ [ ];
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

      channels = { nixpkgs = { }; };

      sharedOverlays = overlays ++ [
        (_: prev: {
          __dontExport = true;
          winklink = winklink.packages."${prev.system}".default;
          hyprland =
            hyprland.packages."${prev.system}".default; # TODO hyprland overlays did not include libdrm in it overlays
          lib = prev.lib.extend
            (_lfinal: _lprev: { mine = import ./lib { inherit lib; }; });
        })

      ];

      nixos = {
        hostDefaults = {
          channelName = "nixpkgs";
          system = "x86_64-linux";

          modules = [
            sops-nix.nixosModules.sops
            digga.nixosModules.nixConfig
            home-manager.nixosModules.home-manager
            nur.nixosModules.nur
            impermanence.nixosModules.impermanence
            xiongchenyu6.nixosModules.bttc
          ];
        };
        imports = [ (digga.lib.importHosts ./hosts/nixos) ];
        hosts = {
          # arm = { system = "aarch64-linux"; };
          mail = {
            modules = [ xiongchenyu6.nixosModules.oci-arm-host-capacity ];
          };
          office = {
            modules = [
              xiongchenyu6.nixosModules.java-tron
              xiongchenyu6.nixosModules.chainlink
              grub2-themes.nixosModules.default
              hyprland.nixosModules.default
              nixos-hardware.nixosModules.lenovo-thinkpad-x1-10th-gen
            ];
          };
          game = {
            modules = [
              hyprland.nixosModules.default
              nixos-hardware.nixosModules.lenovo-legion-16ach6h-hybrid
            ];
          };
        };

        importables = rec {
          profiles = digga.lib.rakeLeaves ./profiles // {
            users = digga.lib.rakeLeaves ./users;
            share = import ./profiles/shares.nix { inherit lib; };
          };
          suites = with profiles; rec {
            base = [ core.nixos sops ];
            common-comps = builtins.attrValues common-components;
            server-comps = builtins.attrValues server-components;
            client-base = base ++ common-comps ++ [
              auto-login.lightdm
              # auto-login.greetd
            ];
            client-network =
              [ common-apps.dn42 common-apps.bird-inner common-apps.kerberos ];
            server-base = base ++ common-comps ++ server-comps ++ [
              server-apps.log.promtail
              server-apps.admin.sssd
              server-apps.monitor.node-exporter
              common-apps.dn42
              common-apps.kerberos
            ];
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
            share = import ./profiles/shares.nix { inherit lib; };
          };
          suites = with profiles; {
            base = [ core.darwin ];
            full = [ core.darwin client-pkgs.darwin ];
          };
        };
      };

      home = {
        modules = [
          nur.hmModules.nur
          hyprland.homeManagerModules.default
          sops-nix.homeManagerModules.sops
          impermanence.nixosModules.home-manager.impermanence
          (import ./profiles/sops.nix)
        ];

        importables = rec {
          profiles = digga.lib.rakeLeaves ./users/profiles // {
            share = import ./profiles/shares.nix { inherit lib; };
          };
          suites = with profiles; {
            nix-remote-build = [ use-remote-builder ];
            cli = [ cli.common cli.shell.zsh ];
            linux-gui = [ gui.nixos gui.mpd ];
            linux-gui-nvidia = [
              cli.common
              cli.shell.zsh
              gui.nixos
              gui.mpd
              gui.window-manager.hyprland.nvidia
            ];
            mac-gui = [ gui.darwin cli.common cli.shell.zsh ];
            dvorak-hyprland = [
              gui.window-manager.hyprland.dvorak
              gui.window-manager.hyprland.nixos
            ];
            xmonad = [ gui.window-manager.xmonad ];
          };
        };
        users = {
          root = { suites, ... }: { imports = suites.nix-remote-build; };
          freeman-cli = { suites, ... }: { imports = suites.cli; };
          freeman-hyprland = { suites, ... }: {
            imports = suites.cli ++ suites.linux-gui ++ suites.dvorak-hyprland;
          };
          freeman-xmonad = { suites, ... }: {
            imports = suites.cli ++ suites.linux-gui ++ suites.xmonad;
          };
          freeman-hyprland-nvidia = { suites, ... }: {
            imports = suites.cli ++ suites.linux-gui-nvidia
              ++ suites.dvorak-hyprland;
          };
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
                deadnix.enable = true;
                shellcheck.enable = true;
                shfmt.enable = true;
              };
            };
        };
      };
    };
}
