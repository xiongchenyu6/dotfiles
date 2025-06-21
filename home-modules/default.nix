{
  inputs,
  ezModules,
  pkgs,
  lib,
  osConfig,
  ...
}:
let
  isDarwin =
    (builtins ? "currentSystem")
    && (builtins.currentSystem == "x86_64-darwin" || builtins.currentSystem == "aarch64-darwin");
  isLinux =
    if (builtins ? "currentSystem") then
      (builtins.currentSystem == "x86_64-linux" || builtins.currentSystem == "aarch64-linux")
    else
      true;
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

  home = {
    packages = with pkgs; [
      home-manager
    ];
  };
}
