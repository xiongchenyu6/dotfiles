# Linux-only GUI configuration: desktop apps, GTK, i18n, Qt, and Linux-specific programs
{
  inputs,
  config,
  pkgs,
  lib,
  ...
}:
{
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
      # Linux-only GUI/desktop
      aspell
      antigravity
      kiro
      aspellDicts.en
      albert
      camber
      baidupcs-go
      sbctl
      #nur.repos.xddxdd.qq
      nur.repos.xddxdd.bilibili
      #nur.repos.xddxdd.dingtalk
      #gimp
      #gitkraken
      # netbird-ui
      #termius
      unrar-wrapper
      moonlight-qt # Sunshine client — stream from sg-office or any Sunshine host
      #whatsapp-for-linux
      wasistlos
      winetricks
      wineWow64Packages.waylandFull
      kdePackages.dolphin
      kdePackages.qtwayland
      kdePackages.qt6ct
      # kdePackages.wayqt
      # kdePackages.qtstyleplugin-kvantum
      feishu-lark
      android-studio
      #opengl-driver
      libsecret
      vulkan-loader
      vulnix
      nix-melt
      godot_4
      blender
      #microsoft-edge
      gotron-sdk
      #vsc-leetcode-cli
      my2sql
      # nix-du
      #pg-ldap-sync
      rustscan
      #stow
      ugm
      rkdeveloptool
      # terraform
      # terraform-ls
      # terracognita
      # terranix
      # terraformer
      # tf2pulumi
      #localstack
      desktop-file-utils
      #inputs.claude-desktop.packages.${system}.claude-desktop-with-fhs
      gnome-software
      gws
      google-cloud-sdk
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
      enable = true;
      extensions = [ "nix" ];
    };

    # password-store = {
    #   enable = true;
    # };
  };
}
