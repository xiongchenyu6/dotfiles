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
      docker
      discord
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
    casks = [ "virtualbox" ];
    global = {
      autoUpdate = true;
      brewfile = true;
    };
    # mac app store
    # masApps = { WireGuard = 1451685025; };
    taps = [ "homebrew/core" "homebrew/cask" "homebrew/cask-drivers" ];
    onActivation = {
      autoUpdate = true;
      cleanup = "zap";
      upgrade = true;
    };
  };

  services = {
    netbird = { enable = true; };
    postgresql = {
      enable = true;
      package = pkgs.postgresql;
      #     enableTCPIP = true;
    };
    redis = {
      enable = true;
      bind = "127.0.0.1";
    };
    skhd = {
      enable = true;
      skhdConfig = ''
        cmd + ctrl - return : open -n -a /Applications/Nix Apps/Alacritty.app
        # focus window
        cmd - h : yabai -m window --focus west
        cmd - j : yabai -m window --focus south
        cmd - k : yabai -m window --focus north
        cmd - l : yabai -m window --focus east
        # move window
        shift + cmd - h : yabai -m window --warp west
        shift + cmd - j : yabai -m window --warp south
        shift + cmd - k : yabai -m window --warp north
        shift + cmd - l : yabai -m window --warp east
        # make floating window fill screen
        shift + alt - up     : yabai -m window --grid 1:1:0:0:1:1
        # fast focus space
        # (done in System Preferences -> Keyboard -> Shortcuts)
        # send window to space and follow focus
        shift + cmd - 1 : yabai -m window --space  1; yabai -m space --focus 1
        shift + cmd - 2 : yabai -m window --space  2; yabai -m space --focus 2
        shift + cmd - 3 : yabai -m window --space  3; yabai -m space --focus 3
        shift + cmd - 4 : yabai -m window --space  4; yabai -m space --focus 4
        shift + cmd - 5 : yabai -m window --space  5; yabai -m space --focus 5
        shift + cmd - 6 : yabai -m window --space  6; yabai -m space --focus 6
        shift + cmd - 7 : yabai -m window --space  7; yabai -m space --focus 7
        shift + cmd - 8 : yabai -m window --space  8; yabai -m space --focus 8
        shift + cmd - 9 : yabai -m window --space  9; yabai -m space --focus 9
        shift + cmd - 0 : yabai -m window --space 10; yabai -m space --focus 10
        # toggle window fullscreen zoom
        alt - f : yabai -m window --toggle zoom-fullscreen
        # float / unfloat window and center on screen
        alt - t : yabai -m window --toggle float;\
                  yabai -m window --grid 4:4:1:1:2:2

      '';
    };

    activate-system.enable = true;

    spacebar = {
      enable = true;
      package = pkgs.spacebar;
      config = {
        position = "top";
        display = "main";
        height = 26;
        title = "on";
        spaces = "on";
        clock = "on";
        power = "on";
        padding_left = 20;
        padding_right = 20;
        spacing_left = 25;
        spacing_right = 15;
        icon_font = ''"Hack Nerd Font:Regular:12.0"'';
        background_color = "0xff202020";
        foreground_color = "0xffa8a8a8";
        power_icon_color = "0xffcd950c";
        battery_icon_color = "0xffd75f5f";
        dnd_icon_color = "0xffa8a8a8";
        clock_icon_color = "0xffa8a8a8";
        power_icon_strip = " ";
        space_icon = "•";
        space_icon_strip = "1 2 3 4 5 6 7 8 9 10";
        spaces_for_all_displays = "on";
        display_separator = "on";
        display_separator_icon = "";
        space_icon_color = "0xff458588";
        space_icon_color_secondary = "0xff78c4d4";
        space_icon_color_tertiary = "0xfffff9b0";
        clock_icon = "";
        dnd_icon = "";
        clock_format = ''"%d/%m/%y %R"'';
        right_shell = "on";
        right_shell_icon = "";
        right_shell_command = "whoami";
      };
    };
    yabai = {
      enable = true;
      enableScriptingAddition = true;
      config = {
        layout = "bsp";

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
  };
  system = {
    defaults = {
      NSGlobalDomain = {
        InitialKeyRepeat = 18;
        KeyRepeat = 6;
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
      trackpad = { Clicking = true; };
    };
    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };
  };
}
