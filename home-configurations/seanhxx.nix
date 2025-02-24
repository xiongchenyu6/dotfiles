{
  pkgs,
  shares,
  config,
  ezModules,
  osConfig,
  lib,
  ...
}:
let
  inherit (pkgs.stdenv) isDarwin;
in
{
  imports =
    lib.debug.traceSeq osConfig.system.nixos.tags (
      if (builtins.elem "gui" osConfig.system.nixos.tags) then
        [
          ezModules.zsh
          ezModules.cli
          ezModules.gui
          ezModules.nvidia
          ezModules.hyprland
        ]
      else
        [ ezModules.tmux ]
    )
    ++ (
      if (builtins.elem "nvidia" osConfig.system.nixos.tags) then
        [
          ezModules.nvidia
        ]
      else
        [ ]
    );

  home = {
    file = {
    };
  };
  programs = {
  };
}
