{
  inputs,
  ezModules,
  pkgs,
  lib,
  osConfig,
  ...
}:
let
  hasNixOSTags = osConfig ? system && osConfig.system ? nixos && osConfig.system.nixos ? tags;
  hasGuiTag = hasNixOSTags && (builtins.elem "gui" osConfig.system.nixos.tags);
  isDarwin = !hasNixOSTags;
  isLinux = hasNixOSTags;
in
{
  imports =
    lib.attrValues {
      inherit (ezModules)
        zsh-minimal
        cli-minimal
        ;
    }
    ++ [
      inputs.sops-nix.homeManagerModules.sops
      inputs.impermanence.homeManagerModules.impermanence
      inputs.vast-cli.homeManagerModules.default
      (import ../shared-modules/sops.nix)
    ]
    ++ (
      if hasGuiTag then
        [
          ezModules.nixos-desktop
        ]
      else
        [ ]
    )
    ++ (
      if isDarwin then
        [
          ./macos-app-fix.nix
        ]
      else
        [ ]
    );

  sops = {
    gnupg = {
      home = "~/.gnupg";
    };
  };

  home = {
    packages = with pkgs; [
      home-manager
    ];
  };
}
