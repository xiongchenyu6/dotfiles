# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ config, pkgs, lib, ... }: {
  imports = [ ./common.nix ];

  qt = { enable = true; };

  home = lib.mkIf pkgs.stdenv.isLinux {
    pointerCursor = {
      name = "Vanilla-DMZ";
      package = pkgs.vanilla-dmz;
      x11 = { enable = true; };
      size = 32;
    };
    packages = with pkgs; [
      aspell
      aspellDicts.en
      config.nur.repos.xddxdd.baidupcs-go
      config.nur.repos.xddxdd.qq
      config.nur.repos.xddxdd.wechat-uos
      gimp
      gitkraken
      ledger-live-desktop
      netbird-ui
      termius
      tdesktop
      unrar-wrapper
      virt-manager
      whatsapp-for-linux
      wineWowPackages.staging
      zotero
      kdePackages.dolphin
      kdePackages.qtwayland
      kdePackages.qt6ct
      # kdePackages.wayqt
      # kdePackages.qtstyleplugin-kvantum
      libsForQt5.oxygen-icons
      feishu-lark
      nil
    ];
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
    wofi = { enable = true; };
    # carapace.enable = true;
    # comodoro.enable = true;
    mpv.enable = true;

    obs-studio = {
      enable = true;
      plugins = with pkgs.obs-studio-plugins; [
        wlrobs
        obs-pipewire-audio-capture
      ];
    };
    thunderbird = {
      enable = true;
      profiles = {
        "xiongchenyu6@gmail.com" = {
          isDefault = true;
          withExternalGnupg = true;
        };
      };
    };

    vscode = {
      enable = true;
      extensions = with pkgs.vscode-marketplace; [
        mkhl.direnv
        juanblanco.solidity
        ms-vscode.cpptools-extension-pack
        ms-vscode.cpptools-themes
        twxs.cmake
        vscjava.vscode-java-debug
        redhat.fabric8-analytics
        vscjava.vscode-java-pack
        github.copilot-chat
        gitlab.gitlab-workflow
        eamodio.gitlens
        weaveworks.vscode-gitops-tools
        golang.go
        visualstudioexptteam.vscodeintellicode
        ms-kubernetes-tools.vscode-kubernetes-tools
        vscjava.vscode-maven
        vscjava.vscode-java-dependency
        ms-python.python
        redhat.java
        github.copilot # AI code completion
        ms-python.python
        vscjava.vscode-java-test
        redhat.vscode-yaml
        ms-azuretools.vscode-docker
        ms-vscode.cpptools
        lfs.vscode-emacs-friendly
        vadimcn.vscode-lldb
        jnoortheen.nix-ide
        mechatroner.rainbow-csv
      ];
    };

    chromium = {
      enable = true;
      package = pkgs.microsoft-edge-dev;
    };

    password-store = { enable = true; };
  };

  services = lib.mkIf pkgs.stdenv.isLinux {
    # safeeyes.enable = true;
    #    ssh-agent.enable = true;
    volnoti = { enable = true; };

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
    #    dropbox = { enable = true; };
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

