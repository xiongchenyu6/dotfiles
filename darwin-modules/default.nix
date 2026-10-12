# Shared Darwin base — imported by all macOS hosts
{
  inputs,
  pkgs,
  ezModules,
  lib,
  shares,
  ...
}:
let
  sharedConfig = import ../shared-modules/default.nix { inherit inputs lib; };

  darwin-modules = with inputs; [
    home-manager.darwinModules.home-manager
    sops-nix.darwinModules.default
    (sharedConfig.mkNixpkgsConfig sharedConfig.baseOverlays)
  ];
in
{
  imports = [
    ezModules.client-cli
    ezModules.client-gui
    ../shared-modules/core.nix
    ../shared-modules/sops.nix
  ]
  ++ darwin-modules;

  system = {
    stateVersion = 6;
  };

  home-manager = sharedConfig.homeManagerConfig;

  security.pam.services.sudo_local.touchIdAuth = true;

  # Remote Login stays on; keys come from shares so every machine with the
  # sops-deployed id_ed25519 or the YubiKey can log in.
  services.openssh = {
    enable = true;
    # Keys only; both Macs are reachable over the mesh and on foreign Wi-Fi.
    extraConfig = ''
      PasswordAuthentication no
      KbdInteractiveAuthentication no
    '';
  };
  users.users."freeman.xiong".openssh.authorizedKeys.keys = [
    shares.users."freeman.xiong".public-key
    shares.users."freeman.xiong".yubikey
  ];
}
