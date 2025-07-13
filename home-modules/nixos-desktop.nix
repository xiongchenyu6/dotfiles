{
  pkgs,
  config,
  lib,
  ...
}:
{

  home = {
    packages = with pkgs; [
      appimage-run
      discord
      freerdp
      dmidecode
      # jetbrains.idea-ultimate
      # jetbrains.rider
      #tectonic
      libreoffice
      stlink-gui
      stm32cubemx
      glib
      gcc-arm-embedded
      google-chrome
      vault
      kmon
      #gitbutler
      #unityhub
      v4l-utils
      pcsc-tools
      opensc
      sui
      #zssh
      record_screen
    ];
  };

  xdg = {
    enable = true;
    #TODO screen capture seems only works in nixos modules, but here for xdg-open
    portal = {
      enable = true;
      xdgOpenUsePortal = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-gtk
        xdg-desktop-portal-hyprland
      ];
      # Add this configuration to address the warning about xdg-desktop-portal 1.17+
      config = {
        common = {
          default = "*";
        };
      };
    };
    mime = {
      enable = true;
    };
    mimeApps = {
      enable = true;
      defaultApplications = {
        "text/html" = "google-chrome.desktop";
        "text/x-csharp" = [ "rider.desktop" ];
        "x-scheme-handler/http" = "google-chrome.desktop";
        "x-scheme-handler/https" = "google-chrome.desktop";
        "x-scheme-handler/about" = "google-chrome.desktop";
        "x-scheme-handler/unknown" = "google-chrome.desktop";
        "x-scheme-handler/claude" = "claude-desktop.desktop";
      };
    };
    userDirs = {
      enable = true;
      createDirectories = true;
      extraConfig = {
        XDG_MISC_DIR = "${config.home.homeDirectory}/Misc";
        XDG_TEST_DIR = "${config.home.homeDirectory}/Test";
        XDG_GIT_DIR = "${config.home.homeDirectory}/Git";
        XDG_PRIVATE_DIR = "${config.home.homeDirectory}/Private";
        XDG_WORKSPACE_DIR = "${config.home.homeDirectory}/Workspace";
      };
    };
  };

  xdg.desktopEntries.claude-desktop = {
    name = "Claude Desktop";
    comment = "Claude AI Desktop Application";
    exec = "claude-desktop %U";
    icon = "claude-desktop";
    terminal = false;
    type = "Application";
    categories = [
      "Network"
      "Chat"
    ];
    mimeType = [ "x-scheme-handler/claude" ];
  };

  programs = {

    chromium = {
      enable = true;
      package = pkgs.google-chrome;
    };

    wofi = {
      enable = true;
    };
  };

  services = {
    dropbox = {
      enable = true;
    };

    pasystray = {
      enable = true;
    };
    poweralertd = {
      enable = true;
    };
    emacs = {
      enable = true;
      defaultEditor = true;
      client = {
        enable = true;
      };
      socketActivation = {
        enable = false;
      };
    };

    dunst = {
      enable = true;
      iconTheme = {
        name = "Adwaita";
        package = pkgs.adwaita-icon-theme;
        size = "16x16";
      };
      settings = {
        global = {
          monitor = 0;
          geometry = "600x50-50+65";
          shrink = "yes";
          transparency = 10;
          padding = 16;
          horizontal_padding = 16;
          font = "JetBrainsMono Nerd Font 10";
          line_height = 4;
          format = "<b>%s</b>\\n%b";
          browser = "${pkgs.xdg-utils}/bin/xdg-open";
          dmenu = "${pkgs.rofi}/bin/rofi -dmenu -i -p dunst";
        };
      };
    };

    blueman-applet = {
      enable = true;
    };
    udiskie = {
      enable = true;
      automount = true;
      notify = true;
      tray = "always";
    };
    syncthing = {
      enable = true;
      tray = {
        enable = true;
      };
    };
  };
  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = [ "qemu:///system" ];
      uris = [ "qemu:///system" ];
    };
  };

}
