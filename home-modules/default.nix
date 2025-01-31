{
  inputs,
  ezModules,
  lib,
  ...
}:
{
  imports =
    lib.attrValues {
      inherit (ezModules)
        # alacritty
        zsh
        cli-minimal
        ;
    }
    ++ [
      inputs.impermanence.nixosModules.home-manager.impermanence
      inputs.sops-nix.homeManagerModules.sops
       (import ../shared-modules/sops.nix)
    ];

  sops = {
    gnupg = {
      home = "~/.gnupg";
    };
  };

  nixpkgs.config = import ../nixpkgs-config.nix;
  xdg.configFile."nixpkgs/config.nix".source = ../nixpkgs-config.nix;
  programs.home-manager.enable = true;
}
