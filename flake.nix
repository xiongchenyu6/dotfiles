{
  description =
    "Flake to manage my laptop, my nur and my hosts on Tencent Cloud";

  inputs = {
    # Core Dependencies
    nixpkgs-stable.url = "github:NixOS/nixpkgs/nixos-22.05";

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
      #url = "github:xiongchenyu6/nur-packages";
      url = "/home/freeman/private/nur-packages";

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

    composer2nix = {
      url = "github:samuelludwig/composer2nix/flakeify";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    gradle2nix = {
      url = "github:randomnetcat/gradle2nix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
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
      };
    };

  };

  outputs = { self, composer2nix, nixpkgs, nixos-hardware, emacs, xddxdd
    , flake-utils, flake-utils-plus, home-manager, agenix, nixos-generators
    , devshell, nixops, nixpkgs-stable, gradle2nix, pre-commit-hooks, nix-alien
    , xiongchenyu6, winklink, deploy-rs, ... }@inputs:
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
      ] ++ [
        (final: prev: {
          composer2nix =
            composer2nix.packages."${prev.system}".composer2nix-noDev;
          krb5Full = prev.krb5Full.overrideAttrs (old: {
            configureFlags = old.configureFlags ++ [ "--with-ldap" ];
          });
          postfix =
            prev.postfix.override { cyrus_sasl = final.cyrus_sasl_with_ldap; };
          cyrus_sasl_with_ldap =
            (prev.cyrus_sasl.override { enableLdap = true; }).overrideAttrs
            (old: {
              postInstall = ''
                ln -sf ${prev.ldap-passthrough-conf}/slapd.conf $out/lib/sasl2/
                ln -sf ${prev.ldap-passthrough-conf}/smtpd.conf $out/lib/sasl2/
              '';
            });
          sssd = prev.sssd.override { withSudo = true; };
          hydra-unstable =
            prev.hydra-unstable.overrideAttrs (old: { doCheck = false; });
          inherit (gradle2nix.packages."${prev.system}") gradle2nix;
          winklink = winklink.packages."${prev.system}".default;
          jdt-language-server = prev.jdt-language-server.overrideAttrs (old: {
            installPhase = let
              # The application ships with config directories for linux and mac
              configDir =
                if prev.stdenv.isDarwin then "config_mac" else "config_linux";
            in ''
              install -D -t $out/share/java/plugins/ plugins/*.jar
              install -Dm 444 -t $out/share/config ${configDir}/*
              launcher="$(ls $out/share/java/plugins/org.eclipse.equinox.launcher_* | sort -V | tail -n1)"
              makeWrapper ${prev.jdk}/bin/java $out/bin/jdtls \
                --add-flags "-Declipse.application=org.eclipse.jdt.ls.core.id1" \
                --add-flags "-Dosgi.bundles.defaultStartLevel=4" \
                --add-flags "-Declipse.product=org.eclipse.jdt.ls.core.product" \
                --add-flags "-Dosgi.sharedConfiguration.area=$out/share/config" \
                --add-flags "-Dosgi.sharedConfiguration.area.readOnly=true" \
                --add-flags "-Dosgi.checkConfiguration=true" \
                --add-flags "-Dosgi.configuration.cascaded=true" \
                --add-flags "-Dlog.level=ALL" \
                --add-flags "-noverify" \
                --add-flags "\$JAVA_OPTS" \
                --add-flags "-jar $launcher" \
                --add-flags "--add-modules=ALL-SYSTEM" \
                --add-flags "--add-opens java.base/java.util=ALL-UNNAMED" \
                --add-flags "--add-opens java.base/java.lang=ALL-UNNAMED"
            '';
          });

        })
      ];
      pkgsFor = system: import nixpkgs { inherit system overlays; };
    in mkFlake {
      inherit self inputs;

      supportedSystems = [ "x86_64-linux" "aarch64-darwin" ];
      #supportedSystems = allSystems;

      channelsConfig = {
        allowUnfree = true;
        allowBroken = true;
      };

      sharedOverlays = overlays;

      hostDefaults = {
        extraArgs = { inherit domain; };
        modules = [
          nixos-hardware.nixosModules.lenovo-thinkpad-x1-9th-gen
          nixos-hardware.nixosModules.common-gpu-intel
          agenix.nixosModule
          home-manager.nixosModules.home-manager
          xiongchenyu6.nixosModules.bttc
        ];
      };

      hosts = {
        office.modules = [ ./hosts/office ];
        mail.modules =
          [ xiongchenyu6.nixosModules.oci-arm-host-capacity ./hosts/tc ];
      };

      outputsBuilder = channels: {
        checks.pre-commit-check =
          pre-commit-hooks.lib."${channels.nixpkgs.system}".run {
            src = ./.;
            hooks = {
              nixfmt.enable = true;
              statix.enable = true;
              nix-linter.enable = true;
            };
          };

        devShells.default = channels.nixpkgs.devshell.mkShell {
          packages = with channels.nixpkgs; [ gopls nix deploy-rs ];
        };
      };
    } // {
      deploy.nodes.mail = {
        sshOpts = [ "-p" "2222" ];
        hostname = "freeman.engineer";
        fastConnection = true;
        profiles = {
          system = {
            sshUser = "root";
            path = deploy-rs.lib.x86_64-linux.activate.nixos
              self.nixosConfigurations.mail;
            user = "root";
          };
        };
      };
    };
}
