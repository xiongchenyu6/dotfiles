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
      cmctl
      cloudflared
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
      kube-capacity
      kube-prompt
      kubectl
      kubectl-tree
      kubespy
      kubeshark
      kustomize
      krew
      kconf
      #korb
      kube-score
      kubelogin-oidc
      calicoctl
      # kdePackages.wayqt
      # kdePackages.qtstyleplugin-kvantum
      libsForQt5.oxygen-icons
      feishu-lark
      websocat
      android-studio
      #opengl-driver
      libsecret
      vulkan-loader
      vulnix
      nix-melt
      #code-cursor
      godot_4
      file
      blender
      delve # go debugger
      dig
      dive # docker image analyzer
      du-dust # du alternative
      envsubst
      fd
      ffmpeg-full
      fluxcd
      glab
      github-copilot-cli
      gitleaks
      weave-gitops
      graphviz
      grpcurl
      #gotron-sdk
      (kubernetes-helm-wrapped.override { plugins = [ kubernetes-helmPlugins.helm-diff ]; })
      #helmify
      hey # http load generator
      killall
      litecli
      #vsc-leetcode-cli
      mongosh
      mycli
      my2sql
      neofetch
      # nix-du
      nix-index-update
      nixpacks
      nixd
      bun
      nvfetcher
      oath-toolkit
      openssl
      #pg-ldap-sync
      popeye
      pgcli
      plantuml
      (python3.withPackages (
        _: with python3.pkgs; [
          pip
          aider-chat
        ]
      ))
      qrencode
      ripgrep
      redis
      rustscan
      ruby
      shellcheck
      shfmt
      s3cmd
      solc-select
      #solium
      sops
      socat
      #stow
      sshpass
      tgpt
      # terraform
      # terraform-ls
      # terracognita
      # terranix
      # terraformer
      # tf2pulumi
      tealdeer
      unzip
      wakatime
      wget
      wrangler
      bun
      yubikey-manager
    ];
    sessionVariables = {
      STARSHIP_LOG = "error";
      NIXPKGS_ALLOW_UNFREE = 1;
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
  programs = {
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
    };

    chromium = {
      enable = true;
      package = pkgs.microsoft-edge;
    };

    password-store = {
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
