# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  config,
  pkgs,
  lib,
  ...
}:
{
  imports = [ ./common.nix ];

  xdg = {
    mimeApps = {
      defaultApplications = {
        "text/html" = "microsoft-edge.desktop";
        "text/x-csharp" = [ "rider.desktop" ];
        "x-scheme-handler/http" = "microsoft-edge.desktop";
        "x-scheme-handler/https" = "microsoft-edge.desktop";
        "x-scheme-handler/about" = "microsoft-edge.desktop";
        "x-scheme-handler/unknown" = "microsoft-edge.desktop";
      };
    };
  };

  qt = {
    enable = true;
  };

  home = lib.mkIf pkgs.stdenv.isLinux {
    pointerCursor = {
      name = "Vanilla-DMZ";
      package = pkgs.vanilla-dmz;
      x11 = {
        enable = true;
      };
      size = 32;
    };
    packages = with pkgs; [
      aspell
      aspellDicts.en
      albert
      # config.nur.repos.xddxdd.baidupcs-go
      # config.nur.repos.xddxdd.qq
      # config.nur.repos.xddxdd.wine-wechat
      gimp
      #gitkraken
      ledger-live-desktop
      netbird-ui
      termius
      tdesktop
      unrar-wrapper
      whatsapp-for-linux
      wineWow64Packages.wayland
      zotero
      kdePackages.dolphin
      kdePackages.qtwayland
      kdePackages.qt6ct
      # kdePackages.wayqt
      # kdePackages.qtstyleplugin-kvantum
      libsForQt5.oxygen-icons
      feishu-lark
      websocat
      # code-cursor
      android-studio
      #aider-chat
      #opengl-driver
      libsecret
      vulkan-loader
      vulnix
      nix-melt
      code-cursor
      godot_4
    ];
    sessionVariables = {
      STARSHIP_LOG = "error";
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
        addons = with pkgs; [
          #fcitx5-mozc
          fcitx5-gtk
          fcitx5-chinese-addons
          fcitx5-rime
        ];
      };
    };
  };
  programs = lib.mkIf pkgs.stdenv.isLinux {
    nh = {
      enable = true;
      flake = "/home/freeman.xiong/dotfiles";
    };
    wofi = {
      enable = true;
    };
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
        twxs.cmake
        gitlab.gitlab-workflow
        eamodio.gitlens
        weaveworks.vscode-gitops-tools
        golang.go
        visualstudioexptteam.vscodeintellicode
        ms-kubernetes-tools.vscode-kubernetes-tools
        ms-python.python
        github.copilot # AI code completion
        redhat.vscode-yaml
        ms-azuretools.vscode-docker
        jnoortheen.nix-ide
        mechatroner.rainbow-csv
        rust-lang.rust-analyzer
        tamasfe.even-better-toml
        github.remotehub
        hediet.vscode-drawio
        ms-vscode.vscode-speech
      ];
    };

    chromium = {
      enable = true;
      package = pkgs.microsoft-edge;
    };

    password-store = {
      enable = true;
    };
  };

  services = lib.mkIf pkgs.stdenv.isLinux {
    # safeeyes.enable = true;
    #    ssh-agent.enable = true;

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
    dropbox = {
      enable = false;
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
