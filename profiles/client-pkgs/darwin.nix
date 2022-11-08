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
      pkgs.emacsGitNativeComp
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

  services = {
    netbird = { enable = true; };
    postgresql = {
      enable = true;
      enableTCPIP = true;
    };
    redis = { enable = true; };
    skhd = {
      enable = true;
      skhdConfig = "alt + shift - r : chunkc quit";
    };
    spacebar = {
      enable = true;
      config = {
        clock_format = "%R";
        background_color = "0xff202020";
        foreground_color = "0xffa8a8a8";
      };
    };
    yabai = {
      enable = true;
      config = {
        focus_follows_mouse = "autoraise";
        mouse_follows_focus = "off";
        window_placement = "second_child";
        window_opacity = "off";
        top_padding = 36;
        bottom_padding = 10;
        left_padding = 10;
        right_padding = 10;
        window_gap = 10;
      };
    };
    system = {
      defaults = {
        NSGlobalDomain = {
          InitialKeyRepeat = 180;
          KeyRepeat = 60;
          AppleTemperatureUnit = "Celsius";
          AppleShowAllFiles = true;
        };
        dock = {
          autohide = true;
          dashboard-in-overlay = true;
          mru-spaces = false;
        };
        finder = {
          AppleShowAllExtensions = true;
          AppleShowAllFiles = true;
          QuitMenuItem = true;
          ShowPathbar = true;
          ShowStatusBar = true;
        };
      };
      keyboard = {
        enableKeyMapping = true;
        remapCapsLockToControl = true;
      };
    };
  };
}
