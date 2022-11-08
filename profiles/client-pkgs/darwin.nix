# Edit this configuration file to define what should be installed on

# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  imports = [ ./common.nix ];

  homebrew = {
    enable = true;
    brews = [{
      name = "mysql@5.6";
      restart_service = true;
      start_service = true;
      link = true;
      conflicts_with = [ "mysql" ];
    }];
    casks = [
      "google-chrome"
      "iterm2"
      "slack"
      "visual-studio-code"
      "docker"
      "virtualbox"
      "postman"
      "dbeaver-community"
      "intellij-idea-ce"
    ];
    global.autoUpdate = true;
    onActivation = {
      autoUpdate = true;
      cleanup = "uninstall";
      upgrade = true;
    };
  };
}
