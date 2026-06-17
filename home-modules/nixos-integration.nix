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
      vault
      kmon
      #gitbutler
      unityhub
      anydesk
      v4l-utils
      pcsc-tools
      opensc
      bitcoin
      sui
      zssh
      record_screen
      # NetworkManager icon themes
      adwaita-icon-theme
      hicolor-icon-theme
    ];
  };

  xdg = {
    enable = true;
    # Portal is configured at the NixOS level in nixos-modules/wayland.nix.
    # Re-declaring it here causes dbus-broker to log "Ignoring duplicate name"
    # for each portal service since home-manager would also add the packages
    # to ~/.nix-profile/share/dbus-1/services/.
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
      commandLineArgs = [
        "--enable-features=VaapiVideoDecoder,VaapiIgnoreDriverChecks,VaapiOnNvidiaGPUs"
        "--disable-features=UseChromeOSDirectVideoDecoder"
      ];
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

    # blueman-applet: do NOT enable here. NixOS's services.blueman.enable
    # (in nixos-modules/misc.nix) already ships the upstream user unit at
    # /etc/systemd/user/blueman-applet.service. Enabling it again here
    # creates a drop-in with a duplicate ExecStart= that systemd refuses
    # ("Service has more than one ExecStart= setting"). The applet is
    # auto-started by graphical-session.target via the upstream WantedBy.
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
      # tray disabled: syncthingtray's replaceable status notifications poison
      # noctalia's popupState (Services/System/NotificationService.qml), making
      # every notification click open syncthingtray. The noctalia bar already
      # surfaces status via the `plugin:syncthing-status` widget.
      tray.enable = false;
    };
    activitywatch = {
      enable = true;
      watchers = {
        # aw-watcher-window-wayland reports to BOTH aw-watcher-window and
        # aw-watcher-afk buckets, so it replaces the X11-only defaults.
        # Niri implements the required protocols (ext-idle-notify-v1 and
        # wlr-foreign-toplevel-management-v1). Heartbeat is hardcoded to 5s;
        # no settings to tune. Must override `package` because home-manager
        # defaults to the `activitywatch` bundle, which only ships the X11
        # watchers (aw-watcher-afk, aw-watcher-window).
        aw-watcher-window-wayland = {
          package = pkgs.aw-watcher-window-wayland;
        };
      };
    };
  };

  # Without explicit ordering, the home-manager activitywatch module starts
  # the wayland watcher as soon as activitywatch.target is reached — which
  # happens before niri.service has set up the wayland socket. The watcher
  # then panics with "Could not find a listening wayland compositor" and
  # exits 101 with no retry. Tie it to graphical-session.target (niri.service
  # has `Before=graphical-session.target`, so the target only fires once the
  # socket is up) and add Restart=on-failure as a safety net for any residual
  # race after the target is reached.
  systemd.user.services."activitywatch-watcher-aw-watcher-window-wayland" = {
    Unit = {
      After = [ "graphical-session.target" ];
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      Restart = "on-failure";
      RestartSec = 5;
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
