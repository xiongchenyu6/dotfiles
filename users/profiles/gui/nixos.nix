# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ config, pkgs, lib, ... }: {
  imports = [ ./common.nix ];
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
        addons = with pkgs; [ fcitx5-mozc fcitx5-gtk fcitx5-chinese-addons fcitx5-rime ];
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
        keyserver = "hkp://keyserver.ubuntu.com";
        fixed-list-mode = true;
        keyid-format = "0xlong";
        list-options = "show-uid-validity";
        cert-digest-algo = "SHA256";
        personal-digest-preferences =  "SHA256";
      };
    };

    chromium = {
      enable = true;
      package = pkgs.brave;
      #google-chrome
    };
  };

  services = lib.mkIf pkgs.stdenv.isLinux {
    safeeyes.enable = true;
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
      enableSshSupport = true;
      sshKeys = [ "6E215C61D97608ED447E9D8BAE448986D75FD8F6" ];
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
