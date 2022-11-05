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
      ] ++ [
        (final: prev: {
          __dontExport = true;
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
    in digga.lib.mkFlake {
      inherit self inputs;

      supportedSystems = [ "x86_64-linux" "aarch64-darwin" ];
      #supportedSystems = allSystems;

      channelsConfig = {
        allowUnfree = true;
        allowBroken = true;
      };

      channels = { nixpkgs = { }; };

      sharedOverlays = overlays;

      nixos = {
        hostDefaults = {
          channelName = "nixpkgs";
          modules = [
            sops-nix.nixosModules.sops

            nixos-hardware.nixosModules.lenovo-thinkpad-x1-9th-gen
            nixos-hardware.nixosModules.common-gpu-intel
            agenix.nixosModule
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
            base = profiles.base;
            client = profiles.client;
            server = profiles.server;
            common-components = builtins.attrValues profiles.common-components;
            common-apps = builtins.attrValues profiles.common-apps;
            client-components = builtins.attrValues profiles.client-components;
            client-apps = builtins.attrValues profiles.client-apps;
            server-apps = builtins.attrValues profiles.server-apps;
            server-components = builtins.attrValues profiles.server-components;
            mysql = profiles.optional-apps.mysql;
            gitea = profiles.optional-apps.gitea;
            healthcheck = profiles.optional-apps.healthcheck;
            calibre-web = profiles.optional-apps.calibre-web;
            gotify-server = profiles.optional-apps.gotify-server;
          };
        };
      };

      devshell = {
        modules = { pkgs, ... }:
          let
            inherit (pkgs)
              agenix cachix editorconfig-checker mdbook nixUnstable nixfmt
              nvfetcher;
            pkgWithCategory = category: package: { inherit package category; };
            devos = pkgWithCategory "devos";
            linter = pkgWithCategory "linter";
            docs = pkgWithCategory "docs";

          in {
            commands = [
              (devos nixUnstable)
              (devos agenix)
              (devos nvfetcher)
              (linter nixfmt)
              (linter editorconfig-checker)

              (docs mdbook)
            ] ++ lib.optionals (!pkgs.stdenv.buildPlatform.isi686)
              [ (devos cachix) ] ++ lib.optionals
              (pkgs.stdenv.hostPlatform.isLinux
                && !pkgs.stdenv.buildPlatform.isDarwin) [
                  (devos inputs.nixos-generators.defaultPackage.${pkgs.system})
                  (devos inputs.deploy-rs.packages.${pkgs.system}.deploy-rs)
                ];
          };
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
