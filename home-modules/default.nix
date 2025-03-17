{
  inputs,
  ezModules,
  pkgs,
  lib,
  osConfig,
  ...
}:
let
  isDarwin = builtins.currentSystem == "x86_64-darwin" || builtins.currentSystem == "aarch64-darwin";
  #isLinux = builtins.currentSystem == "x86_64-linux" || builtins.currentSystem == "aarch64-linux";
  isLinux = false;
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
      if isLinux && (builtins.elem "gui" osConfig.system.nixos.tags) then
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
  home = {
    packages = with pkgs; [
      home-manager
    ];

  };
}
