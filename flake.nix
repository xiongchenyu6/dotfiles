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
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };

    xddxdd = {
      url = "github:xddxdd/nur-packages";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
    };

    nixos-generators = {
      url = "github:nix-community/nixos-generators";
      inputs = {
        nixpkgs.follows = "nixpkgs";
      };
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

  };

  outputs =
    { self
    , nixpkgs
    , nixos-hardware
    , emacs
    , xddxdd
    , flake-utils
    , flake-utils-plus
    , home-manager
    , agenix
    , nixos-generators
    , devshell
    , nixops
    , nixpkgs-stable
    , ...
    } @inputs:
      with nixpkgs;
      with lib;
      with flake-utils.lib;
      with flake-utils-plus.lib;
      let
        overlays = [ devshell.overlay ];
        pkgsFor = system: import nixpkgs { inherit system overlays; };
        domain = "freeman.engineer";
      in
      mkFlake
        {
          inherit self inputs;

          supportedSystems = [ "x86_64-linux" ];
          #supportedSystems = allSystems;

          channelsConfig = {
            allowUnfree = true;
            allowBroken = true;
          };

          sharedOverlays =
            map
              (x: x.overlay or x.overlays.default) [
              agenix
              emacs
              devshell
              self
              xddxdd
            ]
            ++ [
              (final: prev: {
                krb5Full = prev.krb5Full.overrideAttrs (old: {
                  configureFlags = old.configureFlags ++ [
                    "--with-ldap"
                  ];
                });

                cyrus_sasl_with_ldap = (prev.cyrus_sasl.override { enableLdap = true; }).overrideAttrs (old: {
                  postInstall = ''
                    ln -sf ${prev.ldap-passthrough-conf}/slapd.conf $out/lib/sasl2/
                    ln -sf ${prev.ldap-passthrough-conf}/smtpd.conf $out/lib/sasl2/
                  '';
                });
                # nixopsUnstable = (import nixpkgs-stable { system = "x86_64-linux"; }).nixopsUnstable;
              })
            ];

          hostDefaults = {
            extraArgs = {
              inherit domain;
            };
            modules = [
              nixos-hardware.nixosModules.lenovo-thinkpad-x1-9th-gen
              nixos-hardware.nixosModules.common-gpu-intel
              self.nixosModules.bttc
              agenix.nixosModule
              home-manager.nixosModules.home-manager
            ];
          };

          hosts.office.modules = [
            ./host/office
          ];

          # replace 'joes-desktop' with your hostname here.
          outputsBuilder = channels: {
            # Evaluates to `apps.<system>.custom-neovim  = utils.lib.mkApp { drv = ...; exePath = ...; };`.
            # apps = {
            #   custom-neovim = mkApp {
            #     drv = fancy-neovim;
            #     exePath = "/bin/nvim";
            #   };
            # };

            # Evaluates to `packages.<system>.package-from-overlays = <unstable-nixpkgs-reference>.package-from-overlays`.

            packages = import ./pkgs { pkgs = channels.nixpkgs; inherit nixos-generators; inherit (nixpkgs) lib; };

            # Evaluates to `apps.<system>.firefox  = utils.lib.mkApp { drv = ...; };`.
            # defaultApp = mkApp { drv = channels.nixpkgs.firefox; };

            # Evaluates to `defaultPackage.<system>.neovim = <nixpkgs-channel-reference>.neovim`.
            # defaultPackage = channels.nixpkgs.neovim;

            devShells.default = channels.nixpkgs.devshell.mkShell {
              packages = with channels.nixpkgs; [
                # to test with nix (Nix) 2.7.0 and NixOps 2.0.0-pre-7220cbd use
                gopls
                nix
              ];
              imports = [ (channels.nixpkgs.devshell.importTOML ./devshell.toml) ];
            };
            # devShell = channels.nixpkgs.mkShell {
            #   buildInputs = with channels.nixpkgs; [
            #     nix
            #     colmena
            #   ];
            # };

          };
          overlays.default = import ./overlay.nix { inherit nixos-generators lib; };
        } // {
        colmena = {
          meta = {
            nixpkgs = import nixpkgs {
              system = "x86_64-linux";
              overlays = [
                self.overlays.default
                xddxdd.overlay
                emacs.overlay
                (final: prev: {
                  krb5Full = prev.krb5Full.overrideAttrs (old: {
                    configureFlags = old.configureFlags ++ [
                      "--with-ldap"
                    ];
                  });
                  postfix = prev.postfix.override {
                    cyrus_sasl = final.cyrus_sasl_with_ldap;
                  };
                  cyrus_sasl_with_ldap = (prev.cyrus_sasl.override {
                    enableLdap = true;
                  }).overrideAttrs
                    (old: {
                      postInstall = ''
                        ln -sf ${prev.ldap-passthrough-conf}/slapd.conf $out/lib/sasl2/
                        ln -sf ${prev.ldap-passthrough-conf}/smtpd.conf $out/lib/sasl2/
                      '';
                    });
                  sssd = prev.sssd.override
                    { withSudo = true; };
                  hydra-unstable = prev.hydra-unstable.overrideAttrs
                    (old: { doCheck = false; });
                })
              ];
            };
          };
          defaults = {
            imports = [
              agenix.nixosModule
              home-manager.nixosModules.home-manager
            ];
          };
          tc =
            {
              _module.args = {
                inherit domain;
              };

              imports = [
                ./host/tc
              ];

              deployment = {
                targetHost = domain;
                tags = [ "wg" ];
              };
            };
        };
        nixosModules = import ./modules { };
        templates = import ./templates;
      } //
      (

        let
          # System types to support.
          supportedSystems =
            [ "x86_64-linux" ];

          # Helper function to generate an attrset '{ x86_64-linux = f "x86_64-linux"; ... }'.
          forAllSystems = nixpkgs.lib.genAttrs supportedSystems;

          # Nixpkgs instantiated for supported system types.
          nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
        in
        {
          hydraJobs = forAllSystems (system:
            let pkgs = nixpkgsFor.${system};
            in
            {
              tester = self.packages.${system}.default.overrideAttrs (prev: {
                doCheck = true;
                keepBuildDirectory = true;
                #succeedOnFailure = true;
                TESTSUITEFLAGS =
                  "NIX_DONT_SET_RPATH_x86_64_unknown_linux_gnu=1 -x -d";
                checkPhase = ''
                  echo hello
                '';
                postInstall = ''
                  echo hello
                  echo world
                '';
                failureHook = ''
                  test -f tests/testsuite.log && cp tests/testsuite.log $out/
                  test -d tests/testsuite.dir && cp -r tests/testsuite.dir $out/
                '';
              });
              tester-readme = pkgs.runCommand "readme"
                { } ''
                echo hello worl
                mkdir -p $out/nix-support
                echo "# A readme" > $out/readme.md
                echo "doc readme $out/readme.md" >> $out/nix-support/hydra-build-products
              '';
            });
        }
      );
}
