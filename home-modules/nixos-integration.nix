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
      unityhub
      anydesk
      v4l-utils
      pcsc-tools
      opensc
      #sui
      #zssh
      record_screen
      # NetworkManager icon themes
      adwaita-icon-theme
      hicolor-icon-theme
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
        xdg-desktop-portal-gnome
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
        MISC = "${config.home.homeDirectory}/Misc";
        TEST = "${config.home.homeDirectory}/Test";
        GIT = "${config.home.homeDirectory}/Git";
        PRIVATE = "${config.home.homeDirectory}/Private";
        WORKSPACE = "${config.home.homeDirectory}/Workspace";
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
      enable = true; # Temporarily disabled due to build issues
    };

    pasystray = {
      enable = true;
    };
    poweralertd = {
      enable = true;
    };
    # Notification daemon: mako (configured in home-modules/niri)

    blueman-applet = {
      enable = true;
    };
    kdeconnect = {
      enable = true;
      indicator = true;
      package = pkgs.kdePackages.kdeconnect-kde;
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
    activitywatch = {
      enable = true;
      watchers = {
        aw-watcher-afk.settings = {
          timeout = 300;
          poll_time = 2;
        };
        aw-watcher-window.settings = {
          poll_time = 1;
          exclude_title = false;
        };
      };
    };
  };

  # Configure GTK icon theme for nm-applet
  gtk = {
    enable = true;
    iconTheme = {
      name = "Adwaita";
      package = pkgs.adwaita-icon-theme;
    };
  };

  dconf.settings = {
    "org/virt-manager/virt-manager/connections" = {
      autoconnect = [ "qemu:///system" ];
      uris = [ "qemu:///system" ];
    };
    # Set icon theme for nm-applet
    "org/gnome/desktop/interface" = {
      icon-theme = "Adwaita";
    };
  };

}
