# Edit this configuration file to define what should be installed on

# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  imports = [ ./common.nix ];
  environment = {
    systemPackages = with pkgs; [
      postman
      iterm2
      jetbrains.idea-community
      dbeaver
      vscode
    ];
  };
  homebrew = {
    enable = true;
    brews = [{
      name = "mysql";
      restart_service = true;
      start_service = true;
      link = true;
      conflicts_with = [ "mysql" ];
    }];
    casks = [ "visual-studio-code" "docker" "virtualbox" ];
    global.autoUpdate = true;
    onActivation = {
      autoUpdate = true;
      cleanup = "uninstall";
      upgrade = true;
    };
  };
}
