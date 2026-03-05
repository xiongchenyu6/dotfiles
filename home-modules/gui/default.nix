# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  inputs,
  config,
  pkgs,
  lib,
  ...
}:
{
  imports = [ ./common.nix ];

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
    # Fix for winetricks wine64 detection issue
    file.".local/bin/wine64" = {
      source = "${pkgs.wineWow64Packages.waylandFull}/bin/wine";
      executable = true;
    };
    packages = with pkgs; [
      aspell
      antigravity
      kiro
      aspellDicts.en
      albert
      baidupcs-go
      sbctl
      #nur.repos.xddxdd.qq
      nur.repos.xddxdd.bilibili
      nur.repos.xddxdd.wine-wechat
      #nur.repos.xddxdd.dingtalk
      #gimp
      gpgme.dev
      #gitkraken
      cmctl
      cloudflared
      ledger-live-desktop
      netbird-ui
      #termius
      telegram-desktop
      unrar-wrapper
      #whatsapp-for-linux
      wasistlos
      winetricks
      wineWow64Packages.waylandFull
      bottles
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
      #orb
      kube-score
      kubelogin-oidc
      calicoctl
      # kdePackages.wayqt
      # kdePackages.qtstyleplugin-kvantum
      feishu-lark
      websocat
      android-studio
      #opengl-driver
      libsecret
      vulkan-loader
      vulnix
      nix-melt
      godot_4
      file
      blender
      delve # go debugger
      dive # docker image analyzer
      dust # du alternative
      envsubst
      ffmpeg-full
      fluxcd
      glab
      gitleaks
      weave-gitops
      graphviz
      #microsoft-edge
      grpcurl
      gotron-sdk
      (kubernetes-helm-wrapped.override { plugins = [ kubernetes-helmPlugins.helm-diff ]; })
      #helm
      hey # http load generator
      killall
      litecli
      #vsc-leetcode-cli
      mongosh
      #mycli
      my2sql
      # nix-du
      nix-index-update
      nixpacks
      nix
      nvfetcher
      oath-toolkit
      openssl
      #pg-ldap-sync
      popeye
      plantuml
      #aider-chat
      (python3.withPackages (
        _: with python3.pkgs; [
          pip
        ]
      ))
      python312Packages.huggingface-hub.out
      uv
      github-mcp-server
      qrencode
      redis
      rustscan
      ruby
      shellcheck
      shfmt
      ldns
      nmap # A utility for network discovery and security auditing
      ipcalc
      nix-fast-build
      solc-select
      #solium
      sops
      socat
      #stow
      sshpass
      tgpt
      ugm
      rkdeveloptool
      keepassxc
      # terraform
      # terraform-ls
      # terracognita
      # terranix
      # terraformer
      # tf2pulumi
      tealdeer
      unzip
      #localstack
      wakatime-cli
      wget
      #wrangler
      yubikey-manager
      desktop-file-utils
      #inputs.claude-desktop.packages.${system}.claude-desktop-with-fhs
      gnome-software
      tradingview
      # 成像/磁盘工具
      ddrescue
      smartmontools
      hdparm
      util-linux
      coreutils

      # NTFS 只读挂载（可选但建议）
      ntfs3g

      # 取证（命令行）
      sleuthkit

      # 恢复/雕刻（PhotoRec 在 testdisk 包里）
      testdisk
      autopsy
      # 检索与解压与识别
      ripgrep
      p7zip
      unzip
      file
      exiftool

      # 质量提升（可选）
      jq
    ];
    sessionVariables = {
      STARSHIP_LOG = "error";
      NIXPKGS_ALLOW_UNFREE = 1;
    };
    sessionPath = [ "$HOME/.local/bin" ];
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
      type = "fcitx5";
      enable = true;
      fcitx5 = {
        addons = with pkgs; [
          #fcitx5-mozc
          fcitx5-gtk
          #fcitx5-chinese-addons
          fcitx5-rime
        ];
      };
    };
  };
  programs = {
    vastai = {
      enable = true;

      sshConfig = {
        enable = false;
        # Optional: specify API key file path
        # apiKeyFile = /path/to/api/key;
        # Defaults to ~/.config/vastai/vast_api_key
      };
    };

    nix-init = {
      enable = true;
    };

    zathura = {
      enable = true;
    };

    texlive = {
      enable = false;
      extraPackages = tpkgs: {
        inherit (tpkgs)
          collection-basic
          collection-luatex
          collection-langcjk
          collection-latexrecommended
          collection-fontsrecommended
          collection-xetex
          latexmk
          ;
      };
    };

    bun = {
      enable = true;
    };
    # carapace.enable = true;
    # comodoro.enable = true;
    mpv.enable = true;

    obs-studio = lib.mkIf pkgs.stdenv.isLinux {
      enable = true;
      plugins = with pkgs.obs-studio-plugins; [
        wlrobs
        obs-pipewire-audio-capture
      ];
    };
    # thunderbird = {
    #   enable = true;
    #   profiles = {
    #     "xiongchenyu6@gmail.com" = {
    #       isDefault = true;
    #       withExternalGnupg = true;
    #     };
    #   };
    # };

    obsidian = {
      enable = true;
    };

    zed-editor = {
      enable = false;
      extensions = [ "nix" ];
    };

    # password-store = {
    #   enable = true;
    # };
  };
}
