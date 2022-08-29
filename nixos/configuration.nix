# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, options, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./cachix.nix
  ];

  # Bootloader.
  boot.loader = {
    systemd-boot.enable = true;
    efi = {
      canTouchEfiVariables = true;
      efiSysMountPoint = "/boot/efi";

    };
  };
  networking = {
    networkmanager.enable = true;

    hostName = "nixos"; # Define your hostname.
    # Configure network proxy if necessary
    # networking.proxy.default = "http://user:password@proxy:port/";
    # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

    # Enable networking
    #networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  };

  virtualisation.docker.enable = true;

  # Set your time zone.
  time.timeZone = "Asia/Singapore";

  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [ "zh_CN.UTF-8/UTF-8" "en_US.UTF-8/UTF-8" ];
    # Select internationalisation properties.
    inputMethod = {
      enabled = "fcitx5";
      #fcitx.engines = with pkgs.fcitx-engines; [ rime ];
      fcitx5 = {
        #enableRimeData = true;
        addons = with pkgs; [
          fcitx5-chinese-addons
          fcitx5-gtk
          fcitx5-lua
          fcitx5-rime
          fcitx5-with-addons
          fcitx5-configtool
        ];
      };
    };

  };

  # services.xserver.videoDrivers = [ "modsetting" ];

  # Without any `nix.nixPath` entry:
  #  nix.nixPath =
  #    # Prepend default nixPath values.
  #    options.nix.nixPath.default ++
  #    # Append our nixpkgs-overlays.
  #    [ "nixpkgs-overlays=/etc/nixos/overlays-compat/" ]
  #  ;

  #services.xserver.deviceSection = ''
  #  Option "DRI" "2"
  #  Option "TearFree" "true"
  #'';
  #

  services = {
    # go-bttc = {
    #   enable = true;
    # };
    
    xserver = {
      enable = true;
      layout = "us";
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        extraPackages = haskellPackages: [
          haskellPackages.xmonad
          haskellPackages.xmonad-contrib
          haskellPackages.xmonad-extras
          
        ];
      };
      displayManager = {
        lightdm = { enable = true; };
        autoLogin.enable = true;
        autoLogin.user = "freeman";
        defaultSession = "none+xmonad";
        sessionCommands = "${pkgs.xorg.xset}/bin/xset -b";
      };

      # Configure keymap in X11
      xkbOptions = "caps:ctrl_modifier";
      autoRepeatDelay = 180;
      autoRepeatInterval = 60;
      # Enable touchpad support (enabled default in most desktopManager).
      libinput.enable = true;
      # Enable automatic login for the user.
    };
    aria2 = { enable = true; };

    autorandr = {
      enable = true;
      profiles = {
        office = {
          fingerprint = {
            "eDP-1" =
              "00ffffffffffff0009e54c0900000000121e0104a51e1378036980a7544c9825115356000000010101010101010101010101010101019c3e80c870b03c40302036002ebc1000001a000000fd001e3c4a4a10010a202020202020000000fe00424f452043510a202020202020000000fe004e4531343057554d2d4e36320a000a";
            "HDMI-1" =
              "00ffffffffffff001e6dc15bb37c030004200103803c2278ea40b5ae5142ad260f5054210800d1c061404540010101010101010101014dd000a0f0703e803020350058542100001a000000fd00283c1e873c000a202020202020000000fc004c4720554c54524146494e450a000000ff003230344e54464136513533310a01800203427223090707830100004d01030410121f202261605f5e5d6d030c001000b83c20006001020367d85dc401788003e30f0003e2006ae305c000e6060581606050a36600a0f0701f803020350058542100001a565e00a0a0a029503020350058542100001a023a801871382d40582c450058542100001a00000000000000e2";
          };
          config = {
            "eDP-1" = {
              enable = true;
              crtc = 0;
              primary = true;
              position = "0x0";
              mode = "1920x1200";
              rate = "60.00";
            };

            "HDMI-1" = {
              enable = true;
              crtc = 1;
              primary = true;
              position = "1920x0";
              mode = "3840x2160";
              rate = "60.00";
              rotate = "left";
            };

          };
          hooks.postswitch = {
            "polybar-restart" = ''
              polybar-msg cmd restart
            '';
          };
        };
      };
    };

    # Enable CUPS to print documents.
    printing.enable = true;

    gnome.gnome-keyring.enable = true;
    upower.enable = true;

    dbus = {
      enable = true;
      packages = [ pkgs.dconf ];
    };
    # Enable the OpenSSH daemon.
    openssh.enable = true;
    pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;

      };
      pulse.enable = true;
      # If you want to use JACK applications, uncomment this
      #jack.enable = true;

      # use the example session manager (no others are packaged yet so this is enabled by default,
      # no need to redefine it in your config for now)
      #media-session.enable = true;
    };
    blueman.enable = true;

    emacs = {
      package = pkgs.emacsGitNativeComp;
      enable = true;
      defaultEditor = true;
    };
    openvpn.servers = {
      officeVPN = {
        config = "config /home/freeman/Downloads/vpn/vpn/client.ovpn ";
      };
    };

    udev.extraRules = ''
      ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", MODE="0666", RUN+="${pkgs.coreutils}/bin/chmod a+w /sys/class/backlight/%k/brightness"
    '';

    udisks2 = { enable = true; };

    picom = {
      enable = true;
      shadow = true;
      fade = true;
      backend = "glx";
      shadowExclude = [
        "! name~=''"
        "name = 'Notification'"
        "name = 'Plank'"
        "name = 'Docky'"
        "name = 'Kupfer'"
        "name = 'xfce4-notifyd'"
        "name *= 'VLC'"
        "name *= 'compton'"
        "name *= 'Chromium'"
        "name *= 'Chrome'"
        "name *= 'Firefox'"
        "class_g = 'Conky'"
        "class_g = 'Kupfer'"
        "class_g = 'Synapse'"
        "class_g ?= 'Notify-osd'"
        "class_g ?= 'Cairo-dock'"
        "class_g ?= 'Xfce4-notifyd'"
        "class_g ?= 'Xfce4-power-manager'"

      ];

      shadowOpacity = 0.5;
      vSync = true;
      opacityRules = [
        "85:class_g = 'kitty'"
        "85:class_g = 'XTerm'"
        "15:class_g = 'emacs'"
        "90:class_g = 'Wine'"
        "90:class_g = 'Thunderbird'"

      ];
      fadeDelta = 2;
      fadeSteps = [ 1.8e-2 1.8e-2 ];

      wintypes = {
        tooltip = {
          # fade: Fade the particular type of windows.
          fade = true;
          # shadow: Give those windows shadow
          shadow = false;
          # opacity: Default opacity for the type of windows.
          opacity = 0.85;
          # focus: Whether to always consider windows of this type focused.
          focus = true;
        };

      };

    };
    syncthing = {
      enable = true;
      user = "freeman";
      dataDir = "/home/freeman";
      folders = {
        "Office" = {
          enable = true;
          path = "/home/freeman/office";
        };
      };
      };
      
    };

  hardware = {
    pulseaudio.enable = false;
    bluetooth.enable = true;

  };
  systemd = {
    services.upower.enable = true;
    user.services.fcitx5-daemon = {
      enable = false;
      description = "fcitx5";
      unitConfig = { Type = "Simple"; };
      serviceConfig = {
        ExecStart = "fcitx5";
       # Restart = "always";
      };
      wantedBy = [ "graphical-session.target" ];
    };
  };

  # Enable sound with pipewire.
  sound.enable = true;
  security = {
    rtkit.enable = true;
    sudo.enable = true;

  };
  # 

  users.defaultUserShell = pkgs.zsh;

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.freeman = {
    isNormalUser = true;
    description = "freeman";
    extraGroups = [ "networkmanager" "wheel" "video" "audio" ];
    packages = with pkgs;
      [
        #  thunderbird
      ];
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment = {

    systemPackages = with pkgs; [
      autorandr
      automake
      antibody
      awscli2
      b.go-bttc
      brave
      #google-chrome
      clang
      cachix
      conky
      cabal2nix
      consul
      direnv
      dunst
      lua
      nix-direnv
      nixopsUnstable
      neofetch
      dmenu
      dropbox
      ((emacsPackagesFor emacsGitNativeComp).emacsWithPackages (epkgs: [
        epkgs.vterm
        epkgs.org-contrib
        epkgs.org-roam
        epkgs.org-re-reveal
      ]))
      exa
      #  fasd
      feh
      fzf
      gitAndTools.gitflow
      gitAndTools.gitFull
      gitAndTools.hub
      gnupg
      geoip
      gnumake
      gh
      go
      haskell-language-server
      (haskellPackages.ghcWithPackages (self:
        with haskellPackages;
        with pkgs.haskell.lib; [
          #alex
          apply-refact
          cabal-install
          hlint
          stylish-haskell
          hasktags
          hoogle
          #wreq
          xmobar
          #hnix
          #hnix_loc
          #hGelf
          #gender
          hakyll
          #hakyll-sass
          #aeson-pretty
          pandoc
          #hails
        ]))
      hydra_unstable
      imagemagick
      ispell
      jq
      killall
      lsof
      light
      libxml2
      libtool
      libsodium
      python3
      polybar
      wakatime
      #myRepo.example-package
      #neovim
      nixfmt
      node2nix
      nodejs
      nodejs-16_x
      nodePackages."typescript-language-server"
      nodePackages."bash-language-server"
      openssl
      openjdk
      pkgconfig
      protobuf
      snappy
      rsync
      ripgrep
      rnix-lsp
      tree
      tdesktop
      unzip
      vlc
      vim
      stalonetray
      scrot
      stow
      slack
      wget
      which
      #wpa_supplicant_gui
      wakatime
      whatsapp-for-linux
      xclip
      xscreensaver
      zoom
    ];
    pathsToLink = [ "/share/nix-direnv" ];
  };

  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
  '';
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;

  programs = {
    zsh = {
      enable = true;
      shellAliases = {
        vi = "vim";
        yolo = ''git commit -m "$(curl -s whatthecommit.com/index.txt)"'';
        op = "xdg-open";
        ls = "exa --icons";
      };
      ohMyZsh = {
        enable = true;
        plugins = [
          "aws"
          "cabal"
          "catimg"
          "colored-man-pages"
          "colorize"
          "command-not-found"
          "copyfile"
          "docker"
          "docker-compose"
          "direnv"
          "extract"
          "encode64"
          "emacs"
          "fzf"
          "fancy-ctrl-z"
          "git"
          "git-flow"
          "git-auto-fetch"
          "git-hubflow"
          "github"
          "gitignore"
          "gpg-agent"
          "golang"
          "httpie"
          "heroku"
          "jsontools"
          "kubectl"
          "npm"
          "node"
          "pass"
          "pipenv"
          "pip"
          "ripgrep"
          "redis-cli"
          "sbt"
          "scala"
          "systemd"
          "tmux"
        ];
      };
      setOptions = [
        "BANG_HIST"
        "EXTENDED_HISTORY"

        "INC_APPEND_HISTORY"
        "SHARE_HISTORY"
        "HIST_EXPIRE_DUPS_FIRST"
        "HIST_IGNORE_DUPS"
        "HIST_IGNORE_ALL_DUPS"
        "HIST_FIND_NO_DUPS"
        "HIST_IGNORE_SPACE"
        "HIST_SAVE_NO_DUPS"
        "HIST_REDUCE_BLANKS"
        "HIST_VERIFY"
        "HIST_BEEP"

      ];
      enableCompletion = true;
      autosuggestions = {
        enable = true;

      };
      syntaxHighlighting = {
        enable = true;

      };
      enableBashCompletion = true;
    };
    ssh.startAgent = true;
    gnupg.agent = { enable = true; };
    git = {
      enable = true;
      lfs = {
        enable = true;

      };
    };
    tmux = {
      enable = true;
      terminal = "screen-256color";
      shortcut = "space";
      plugins = with pkgs.tmuxPlugins; [ yank ];
      secureSocket = false;
      keyMode = "vi";

    };
    htop = { enable = true; };
    nm-applet.enable = true;
    starship = {
      enable = true;
      settings = {
        format = "$directory$character";
        # move the rest of the prompt to the right
        right_format = "$all";
        # A continuation prompt that displays two filled in arrows
        continuation_prompt = "▶▶";
        kubernetes.disabled = false;
        directory = {
          truncation_length = 20;
          truncation_symbol = "…/";
        };
        status.disabled = false;
        time.disabled = false;
        git_metrics.disabled = false;
        sudo.disabled = false;
      };
    };
  };

  # List services that you want to enable:

  fonts = {
    fontconfig.enable = true;
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      wqy_microhei
      wqy_zenhei
      (nerdfonts.override { fonts = [ "Hack" ]; })
      jetbrains-mono
    ];
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.05"; # Did you read the comment?

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

}
