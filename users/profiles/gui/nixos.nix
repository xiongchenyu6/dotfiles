# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ config, pkgs, lib, ... }: {
  imports = [ ./common.nix ];

  qt = {
    enable = true;

  };
  xdg = {
    enable = true;
    mime = { enable = true; };
    mimeApps = { enable = true; };
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
  home = lib.mkIf pkgs.stdenv.isLinux {
    pointerCursor = {
      name = "Vanilla-DMZ";
      package = pkgs.vanilla-dmz;
      x11 = { enable = true; };
      size = 32;
    };
    sessionVariables = { STARSHIP_LOG = "error"; };
  };

  gtk = lib.mkIf pkgs.stdenv.isLinux {
    enable = true;
    gtk4 = {
      extraConfig = {
        gtk-cursor-blink = false;
        gtk-recent-files-limit = 20;
      };
    };
  };

  i18n = lib.mkIf pkgs.stdenv.isLinux {
    inputMethod = {
      enabled = "fcitx5";
      fcitx5 = {
        addons = with pkgs; [
          fcitx5-mozc
          fcitx5-gtk
          fcitx5-chinese-addons
          fcitx5-rime
        ];
      };
    };
  };

  programs = lib.mkIf pkgs.stdenv.isLinux {
    obs-studio = {
      enable = true;
      plugins = with pkgs.obs-studio-plugins; [
        wlrobs
        obs-pipewire-audio-capture
      ];
    };
    gpg = {
      enable = true;
      settings = {
        # keyserver = "hkps://keyserver.ubuntu.com";
        fixed-list-mode = true;
        keyid-format = "0xlong";
        list-options = "show-uid-validity";
        # cert-digest-algo = "SHA256";
        # personal-digest-preferences = "SHA256";
      };
    };

    vscode = {
      enable = true;
      extensions = with pkgs.vscode-extensions; [
        mkhl.direnv
        golang.go
        bbenoist.nix
        redhat.java
        github.copilot # AI code completion
        ms-python.python
        ms-vscode.cpptools
        vscodevim.vim
        vadimcn.vscode-lldb
      ];
    };

    chromium = {
      enable = true;
      package = pkgs.brave;
      #google-chrome
    };

    password-store = { enable = true; };
  };

  services = lib.mkIf pkgs.stdenv.isLinux {
    # safeeyes.enable = true;
    pasystray = { enable = true; };
    poweralertd = { enable = true; };
    emacs = {
      enable = true;
      defaultEditor = true;
      client = { enable = true; };
      socketActivation = { enable = false; };
    };

    dunst = {
      enable = true;
      iconTheme = {
        name = "Adwaita";
        package = pkgs.gnome.adwaita-icon-theme;
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

    blueman-applet = { enable = true; };
    dropbox = { enable = true; };
    gpg-agent = {
      enable = true;
      enableExtraSocket = true;
      # extraConfig = ''
      #   allow-emacs-pinentry
      #   allow-loopback-pinentry
      # '';
      # pinentryFlavor = "emacs";
      enableSshSupport = true;
      # gpg2 -K --with-keygrip 
      sshKeys = [
        "6E215C61D97608ED447E9D8BAE448986D75FD8F6"
        "42C87EA7DAAD37765EB1DD0FF53339EFBBF5785C"
      ];
    };
    udiskie = {
      enable = true;
      automount = true;
      notify = true;
      tray = "always";
    };
    syncthing = {
      enable = true;
      tray = { enable = true; };
    };
  };
}
