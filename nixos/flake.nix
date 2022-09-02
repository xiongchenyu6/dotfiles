{
  description = "Flake to manage my laptop and my hosts on Tencent Cloud";

  inputs = {
    # Core Dependencies
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    utils.url = "github:numtide/flake-utils";

    emacs.url = "github:nix-community/emacs-overlay";

    myRepo = {
      url = "/home/freeman/private/nur-packages";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    xddxdd = {
      url = "github:xddxdd/nur-packages";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    bttc = {
      #url = "github:xiongchenyu6/bttc";
      url = "/home/freeman/private/bttc";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self, nixpkgs, nixos-hardware, emacs, myRepo, xddxdd, bttc, utils, ... }:
    let pkgsFor = system: import nixpkgs { inherit system; };
    in with nixpkgs;
    {
      # replace 'joes-desktop' with your hostname here.
      nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          nixos-hardware.nixosModules.lenovo-thinkpad-x1-9th-gen
          nixos-hardware.nixosModules.common-gpu-intel
          bttc.nixosModules.bttc
          ./configuration.nix
          ({ pkgs, ... }: {
            nixpkgs.overlays = [
              emacs.overlay
              (final: prev: {
                myRepo = myRepo.packages."${prev.system}";
                xddxdd = xddxdd.packages."${prev.system}";
                b = bttc.packages."${prev.system}";
                nix-direnv = prev.nix-direnv.override { enableFlakes = true; };
              })
            ];
          })
        ];
      };

      nixopsConfigurations = with lib; {
        default = rec {
          inherit nixpkgs;
          network.storage.legacy.databasefile = "~/.nixops/deployments.nixops";
          network.description = "tron sg";
          network.enableRollback = true;
          prometheus = rec {
            _module.args = with nixpkgs.lib; {
              inherit region;
              #  awsConfig = traceVal(fromTOML (builtins.readFile /home/freeman/.aws/credentials.toml));
            };
            # imports = [ ./ec2-info.nix ./prometheus.nix ./node-exporter.nix ];
          };
        };
      };
    } // utils.lib.eachDefaultSystem (system:
      let pkgs = pkgsFor system;
      in {
        defaultPackage = pkgs.hello;

        # used by nix develop and nix shell
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            # to test with nix (Nix) 2.7.0 and NixOps 2.0.0-pre-7220cbd use
            nix
            nixopsUnstable
          ];
        };
      });
}
