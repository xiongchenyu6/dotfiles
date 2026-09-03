{
  lib,
  osConfig,
  ...
}:
let
  hasNixOSTags = osConfig ? system && osConfig.system ? nixos && osConfig.system.nixos ? tags;
  hasNvidiaTag = hasNixOSTags && builtins.elem "nvidia" osConfig.system.nixos.tags;
  hasNvidiaOffload = hasNixOSTags && (osConfig.hardware.nvidia.prime.offload.enable or false);
in
{
  imports = [
    ./zsh.nix
    ./cli-development.nix
    ./gui
    ./stow-config.nix
    ./niri
    ./tmux.nix
    ./lan-mouse.nix
  ]
  ++ lib.optionals (hasNvidiaTag && !hasNvidiaOffload) [ ./nvidia.nix ];
}
