{
  description = "NixOS configuration with flakes";

  inputs.nixpkgs.url = github:NixOS/nixpkgs/nixos-unstable;

  inputs.nixos-hardware.url = github:NixOS/nixos-hardware/master;

  inputs.flake-utils.url = github:numtide/flake-utils;

  inputs.emacs.url = github:nix-community/emacs-overlay;


  outputs = { self, nixpkgs, nixos-hardware, flake-utils, emacs }: {
    # replace 'joes-desktop' with your hostname here.
    nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
      system = "x86_64-linux";
      modules = [ 
       nixos-hardware.nixosModules.lenovo-thinkpad-x1-9th-gen
       nixos-hardware.nixosModules.common-gpu-intel
       ./configuration.nix 
       ({ pkgs, ... }: {
         nixpkgs.overlays = [
           emacs.overlay
           (self: super: { nix-direnv = super.nix-direnv.override { enableFlakes = true; }; } )
         ];
        })
      ];
    };
  };
}
