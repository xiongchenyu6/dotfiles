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

  };

  outputs = { self, composer2nix, nixpkgs, nixos-hardware, emacs, xddxdd
    , flake-utils, flake-utils-plus, home-manager, agenix, nixos-generators
    , devshell, nixops, nixpkgs-stable, xiongchenyu6, ... }@inputs:
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
      ] ++ [
        (final: prev: {
          composer2nix =
            composer2nix.packages."x86_64-linux".composer2nix-noDev;
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
          calibre-web = prev.calibre-web.overrideAttrs (old: {
            buildInputs = old.propagatedBuildInputs
              ++ (with prev.python3.pkgs; [
                python-ldap
                prev.Flask-SimpleLDAP
              ]);
            postPatch = ''
              mkdir -p src/calibreweb
              mv cps.py src/calibreweb/__init__.py
              mv cps src/calibreweb
              substituteInPlace setup.cfg \
                --replace "cps = calibreweb:main" "calibre-web = calibreweb:main" \
                --replace "chardet>=3.0.0,<4.1.0" "chardet>=3.0.0,<6" \
                --replace "Flask>=1.0.2,<2.1.0" "Flask>=1.0.2" \
                --replace "Flask-Login>=0.3.2,<0.6.2" "Flask-Login>=0.3.2" \
                --replace "flask-wtf>=0.14.2,<1.1.0" "flask-wtf>=0.14.2" \
                --replace "lxml>=3.8.0,<4.9.0" "lxml>=3.8.0" \
                --replace "tornado>=4.1,<6.2" "tornado>=4.1,<7" \
                --replace "PyPDF3>=1.0.0,<1.0.7" "PyPDF3>=1.0.0" \
                --replace "requests>=2.11.1,<2.28.0" "requests" \
                --replace "unidecode>=0.04.19,<1.4.0" "unidecode>=0.04.19" \
                --replace "python-ldap>=3.0.0,<3.5.0" "python-ldap>=3.0.0" \
                --replace "Flask-SimpleLDAP>=1.4.0,<1.5.0" "Flask-SimpleLDAP>=1.4.0" \
                --replace "werkzeug<2.1.0" ""
            '';
          });
        })
      ];
      pkgsFor = system: import nixpkgs { inherit system overlays; };
    in (mkFlake {
      inherit self inputs;

      supportedSystems = [ "x86_64-linux" ];
      #supportedSystems = allSystems;

      channelsConfig = {
        allowUnfree = true;
        allowBroken = true;
      };

      lib = import ./lib { lib = digga.lib // nixos.lib; };

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

      hosts.office.modules = [ ./hosts/office ];

      outputsBuilder = channels: {
        devShells.default = channels.nixpkgs.devshell.mkShell {
          packages = with channels.nixpkgs; [ gopls nix ];
          imports = [ (channels.nixpkgs.devshell.importTOML ./devshell.toml) ];
        };
      };
    } // {
      colmena = {
        meta = {
          nixpkgs = import nixpkgs {
            system = "x86_64-linux";
            inherit overlays;
          };
        };
        defaults = {
          imports = [
            agenix.nixosModule
            home-manager.nixosModules.home-manager
            xiongchenyu6.nixosModules.oci-arm-host-capacity
          ];
        };
        tc = {
          imports = [ ./hosts/tc ];

          deployment = {
            targetHost = "freeman.engineer";
            tags = [ "wg" ];
          };
        };
      };
    });
}
