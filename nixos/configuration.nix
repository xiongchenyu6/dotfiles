# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, options, lib, ... }:

{
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ./cachix.nix
  ];

  # Bootloader.
  boot = {
    tmpOnTmpfs = lib.mkDefault true;
    loader = {
      systemd-boot = {
        enable = true;
        editor = false;
      };
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/efi";

      };
    };
    kernelModules = [ "tcp_bbr" ];
    kernel.sysctl = {
      # The Magic SysRq key is a key combo that allows users connected to the
      # system console of a Linux kernel to perform some low-level commands.
      # Disable it, since we don't need it, and is a potential security concern.
      "kernel.sysrq" = 511;

      ## TCP hardening
      # Prevent bogus ICMP errors from filling up logs.
      "net.ipv4.icmp_ignore_bogus_error_responses" = 1;
      # Reverse path filtering causes the kernel to do source validation of
      # packets received from all interfaces. This can mitigate IP spoofing.
      "net.ipv4.conf.default.rp_filter" = 1;
      "net.ipv4.conf.all.rp_filter" = 1;
      # Do not accept IP source route packets (we're not a router)
      "net.ipv4.conf.all.accept_source_route" = 0;
      "net.ipv6.conf.all.accept_source_route" = 0;
      # Don't send ICMP redirects (again, we're on a router)
      "net.ipv4.conf.all.send_redirects" = 0;
      "net.ipv4.conf.default.send_redirects" = 0;
      # Refuse ICMP redirects (MITM mitigations)
      "net.ipv4.conf.all.accept_redirects" = 0;
      "net.ipv4.conf.default.accept_redirects" = 0;
      "net.ipv4.conf.all.secure_redirects" = 0;
      "net.ipv4.conf.default.secure_redirects" = 0;
      "net.ipv6.conf.all.accept_redirects" = 0;
      "net.ipv6.conf.default.accept_redirects" = 0;
      # Protects against SYN flood attacks
      "net.ipv4.tcp_syncookies" = 1;
      # Incomplete protection again TIME-WAIT assassination
      "net.ipv4.tcp_rfc1337" = 1;

      ## TCP optimization
      # TCP Fast Open is a TCP extension that reduces network latency by packing
      # data in the sender’s initial TCP SYN. Setting 3 = enable TCP Fast Open for
      # both incoming and outgoing connections:
      "net.ipv4.tcp_fastopen" = 3;
      # Bufferbloat mitigations + slight improvement in throughput & latency
      "net.ipv4.tcp_congestion_control" = "bbr";
      "net.core.default_qdisc" = "cake";
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

  virtualisation = { docker.enable = true; };

  # Set your time zone.
  time = {
    timeZone = "Asia/Singapore";

  };

  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [ "zh_CN.UTF-8/UTF-8" "en_US.UTF-8/UTF-8" ];
    # Select internationalisation properties.
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
    cachix-agent = {
      enable = true;
      credentialsFile = ./cachix.secret;
      verbose = true;
      # cachix = [ "nixos" ];
    };

    xserver = {
      enable = true;
      layout = "us";
      displayManager = {
        lightdm = { enable = true; };
        autoLogin.enable = true;
        autoLogin.user = "freeman";
        session = [{
          manage = "desktop";
          name = "xsession";
          start = "exec $HOME/.xsession";
        }];
        defaultSession = "xsession";
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

    # Enable CUPS to print documents.
    printing.enable = true;

    gnome.gnome-keyring.enable = true;
    upower.enable = true;

    dbus = { enable = true; };
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

    postgresql = {
      enable = true;
      ensureUsers = [{ name = "freeman"; }];
    };

    hydra = {
      enable = true;
      hydraURL = "http://localhost:3000"; # externally visible URL
      notificationSender = "hydra@localhost"; # e-mail of hydra service
      # a standalone hydra will require you to unset the buildMachinesFiles list to avoid using a nonexistant /etc/nix/machines
      buildMachinesFiles = [ ];
      # you will probably also want, otherwise *everything* will be built from scratch
      useSubstitutes = true;
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
  sound = { enable = true; };

  security = {
    rtkit.enable = true;
    sudo.enable = true;
    acme.acceptTerms = true;
  };
  # 

  users = {
    defaultUserShell = pkgs.zsh;
    users = {
      freeman = {
        shell = pkgs.zsh;
        isNormalUser = true;
        description = "freeman";
        extraGroups = [
          "networkmanager"
          "wheel"
          "video"
          "audio"
          "cdrom"
          "disk"
          "floppy"
          "scanner"
          "storage"
          "power"
          "dialout"
          "plugdev"
          "lp"
          "input"
          "socket"
          "spi"
          "bus"
          "dropbox"
        ];
        packages = with pkgs;
          [
            #  thunderbird
          ];
      };
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.

  # Allow unfree packages
  nixpkgs = { config.allowUnfree = true; };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment = {

    systemPackages = with pkgs; [
      automake
      antibody
      asciinema
      awscli2
      brave
      #google-chrome
      clang
      conky
      cabal2nix
      cachix
      consul
      discord
      dig
      lua
      nix-direnv
      nixopsUnstable
      neofetch
      dmenu
      #dropbox
      ((emacsPackagesFor emacsGitNativeComp).emacsWithPackages (epkgs: [
        epkgs.vterm
        epkgs.org-contrib
        epkgs.org-roam
        epkgs.org-re-reveal
        epkgs.pdf-tools
        epkgs.leetcode
      ]))
      exa
      #  fasd
      feh
      gitAndTools.gitflow
      gitAndTools.hub
      git-crypt
      #      gnupg
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
      heroku
      hydra_unstable
      imagemagick
      ispell
      killall
      lsof
      light
      libxml2
      libtool
      libsodium
      (python3.withPackages (ps: [ myRepo.my_cookies ]))
      pinentry
      linuxPackages.ply
      polybar
      wakatime
      #myRepo.example-package
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
      tomb
      tcpdump
      unzip
      vlc
      stalonetray
      scrot
      stow
      slack
      wget
      which
      wineWowPackages.staging
      #wpa_supplicant_gui
      wakatime
      whatsapp-for-linux
      xclip
      xddxdd.qq
      xddxdd.wechat-uos
      xscreensaver
      zoom-us
    ];
  };

  nix = {
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
    settings.experimental-features = [ "nix-command" "flakes" ];
    settings = {
      trusted-users = [ "root" "freeman" ];
    };
  };
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;

  programs = {
    #ssh.startAgent = true;
    #gnupg = { agent = { enable = true; }; };
    atop = {
      enable = true;
      netatop = { enable = true; };
      atopgpu = { enable = true; };
    };
    nm-applet.enable = true;
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
  system = { stateVersion = "22.11"; }; # Did you read the comment?

}
