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
    defaultLocale = "en_SG.utf8";
    # Select internationalisation properties.
    inputMethod = {
      enabled = "fcitx";
      fcitx.engines = with pkgs.fcitx-engines; [ rime ];
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
        sessionCommands = ''${pkgs.xorg.xset}/bin/xset r rate 180 60'';
     };

      # Configure keymap in X11
      xkbOptions = "caps:ctrl_modifier";
      # Enable touchpad support (enabled default in most desktopManager).
      libinput.enable = true;
      # Enable automatic login for the user.
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

        udisk2 = {
         enable = true;
         };

         picom = {
         enable =true;
         };
  };
  hardware = {
    pulseaudio.enable = false;
    bluetooth.enable = true;

  };
  systemd.services.upower.enable = true;

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
      aria2
      arandr
      automake
      antibody
      brave
      #google-chrome
      clang
      cachix
      conky
      cabal2nix
      direnv
      dunst
      nix-direnv
      dmenu
      dropbox
      ((emacsPackagesFor emacsGitNativeComp).emacsWithPackages (epkgs: [
        epkgs.vterm
        epkgs.org-contrib
        epkgs.org-roam
        epkgs.org-re-reveal
      ]))
      exa
      fasd
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
      htop
      imagemagick
      ispell
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
      jq
      lsof
      light
      libxml2
      libtool
      libsodium
      python3
      polybar
      wakatime
      neovim
      nixfmt
      node2nix
      nodejs
      nodePackages."typescript-language-server"
      openssl
      openjdk
      pkgconfig
      protobuf
      snappy
      rxvt_unicode
      rsync
      ripgrep
      tmux
      tree
      tdesktop
      unzip
      vlc
      stalonetray
      scrot
      stow
      slack
      starship
      wget
      which
      wpa_supplicant_gui
      wakatime
      xclip
      xscreensaver
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
    zsh.enable = true;
    ssh.startAgent = true;
    gnupg.agent = { enable = true; };
    tmux.enable = true;
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
  system.stateVersion = "22.05"; # Did you read the comment?

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

}
