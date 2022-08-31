{
  description = "NixOS configuration with flakes";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    emacs.url = "github:nix-community/emacs-overlay";

    myRepo = {
      url = "github:xiongchenyu6/nur-packages";
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

  outputs = { self, nixpkgs, nixos-hardware, emacs, myRepo, xddxdd,bttc }: {
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
    nixopsConfigurations.sgServer =nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
    };
  };
}
