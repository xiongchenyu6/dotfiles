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
    ++ [
      inputs.sops-nix.homeManagerModules.sops
      inputs.impermanence.homeManagerModules.impermanence
      inputs.vast-cli.homeManagerModules.default
    ]
    ++ (
      if hasGuiTag then
        [
          ezModules.nixos-integration
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

  sops = lib.mkIf (!isRoot) {
    defaultSopsFile = ../secrets/common.yaml;
    gnupg.home = "~/.gnupg";
  };

  home = {
    packages = with pkgs; [
      home-manager
    ];
  };
}
