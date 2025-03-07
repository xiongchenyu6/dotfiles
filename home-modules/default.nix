{
  inputs,
  ezModules,
  lib,
  ...
}:
let
  isDarwin = builtins.currentSystem == "x86_64-darwin" || builtins.currentSystem == "aarch64-darwin";
  isLinux = builtins.currentSystem == "x86_64-linux" || builtins.currentSystem == "aarch64-linux";
in
{
  imports =
    lib.attrValues {
      inherit (ezModules)
        # alacritty
        zsh-minimal
        cli-minimal
        ;
    }
    ++ [
      inputs.impermanence.nixosModules.home-manager.impermanence
      inputs.sops-nix.homeManagerModules.sops
      (import ../shared-modules/sops.nix)
    ]
    ++ (
      if isLinux then
        [
          ezModules.nixos-desktop
        ]
      else
        [ ]
    );

  sops = {
    gnupg = {
      home = "~/.gnupg";
    };
  };

  #xdg.configFile."nixpkgs/config.nix".source = ../nixpkgs-config.nix;
  programs.home-manager.enable = true;
}
