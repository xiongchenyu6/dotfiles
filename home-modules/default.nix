{
  inputs,
  ezModules,
  pkgs,
  lib,
  config,
  osConfig,
  ...
}:
let
  hasNixOSTags = osConfig ? system && osConfig.system ? nixos && osConfig.system.nixos ? tags;
  hasGuiTag = hasNixOSTags && (builtins.elem "gui" osConfig.system.nixos.tags);
  isDarwin = !hasNixOSTags;
  isRoot = config.home.username == "root";
in
{
  imports =
    lib.attrValues {
      inherit (ezModules)
        zsh-minimal
        cli-minimal
        ;
    }
    ++ lib.optionals (!isRoot) [
      inputs.sops-nix.homeManagerModules.sops
      (import ../shared-modules/sops.nix)
      {
        sops.gnupg.home = "~/.gnupg";
      }
    ]
    ++ [
      inputs.impermanence.homeManagerModules.impermanence
      inputs.vast-cli.homeManagerModules.default
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

  home = {
    packages = with pkgs; [
      home-manager
    ];
  };
}
