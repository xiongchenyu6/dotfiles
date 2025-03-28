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
      onlyoffice-bin
      stlink-gui
      stm32cubemx
      glib
      gcc-arm-embedded
      microsoft-edge
      vault
      kmon
      #gitbutler
      unityhub
      v4l-utils
      pcsc-tools
      opensc
      sui
      zssh
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
        "text/html" = "microsoft-edge.desktop";
        "text/x-csharp" = [ "rider.desktop" ];
        "x-scheme-handler/http" = "microsoft-edge.desktop";
        "x-scheme-handler/https" = "microsoft-edge.desktop";
        "x-scheme-handler/about" = "microsoft-edge.desktop";
        "x-scheme-handler/unknown" = "microsoft-edge.desktop";
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

  programs = {

    chromium = {
      enable = true;
      package = pkgs.microsoft-edge;
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
