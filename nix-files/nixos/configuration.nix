# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
#sand in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:


let
   nixos-hardware = builtins.fetchTarball https://github.com/azazel75/nixos-hardware/archive/master.tar.gz;
   nixpkgs-mozilla = builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz;
  
in

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      "${nixos-hardware}/lenovo/thinkpad/x1/6th-gen/default.nix"
    ];

  # Use the systemd-boot EFI boot loader.
  nixpkgs.config.allowUnsupportedSystem = true; 
  nixpkgs.config.allowBroken = true; 
  nixpkgs.config.allowUnfree = true;  
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "nixos"; # Define your hostname.
  networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
  virtualisation.docker.enable = true ;
  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };
  i18n.inputMethod = {
    enabled = "fcitx" ;
    fcitx.engines = with pkgs.fcitx-engines; [ rime ] ;
  };

  # Set your time zone.
  time.timeZone = "Asia/Singapore";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    aria2 arandr automake
    chromium ctags clang
    dmenu docker_compose dropbox
    emacs
    fasd feh
    gitAndTools.gitflow gitAndTools.gitFull gitAndTools.hub gnupg global geoip
    gnumake imagemagick ispell
    (haskellPackages.ghcWithPackages (self : with haskellPackages; with pkgs.haskell.lib; [
      #alex
      apply-refact
      cabal-install
      hlint
      stylish-haskell
      hasktags
      hoogle
      #wreq
      xmobar
      taffybar
      #hnix
      #hnix_loc
      #hGelf
      #gender
      hakyll
      #hakyll-sass
      #aeson-pretty
      pandoc
      stack
      summoner
      #hails
    ]))
    lsof light libxml2 libtool libsodium
    mu
    python3
    wakatime
    neovim nix-prefetch-git
    openssl openjdk
    pkgconfig protobuf
    sudo snappy
    rxvt_unicode rsync
    tmux tree thunderbird
    unzip
    stalonetray skype scrot stow
    wget which wpa_supplicant_gui
    xclip xscreensaver xlibs.xev xlibs.xmodmap
    xfce.xfce4-power-manager
    zsh
  ];

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  nix.binaryCaches = [ "https://cache.nixos.org/" "https://nixcache.reflex-frp.org" ];
  nix.binaryCachePublicKeys = [ "ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI=" ];

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;
  hardware.pulseaudio.support32Bit = true; 
  virtualisation.virtualbox.host.enable = true; 
  programs.zsh.enable = true;
  programs.zsh.autosuggestions.enable = true;
  programs.zsh.enableCompletion = true;
  programs.zsh.ohMyZsh.enable = true;
  programs.zsh.ohMyZsh.plugins =
     [ "git"
       "git-auto-fetch"
       "git-extras"
       "git-flow"
       "gitignore"
       "fasd"
       "emacs"
       "tmux"
       "stack"
       "aws"
       "docker"
       "docker-compose"
       "colored-man-pages"
       "zsh-wakatime"
       "extract"
      ];
  programs.zsh.ohMyZsh.theme = "avit";
  programs.ssh.startAgent = true;
  programs.adb.enable = true;
  services.neo4j = {
     enable = true;
     directories.home = "/var/db/neo4j";
     package = pkgs.neo4j;
  };
  services.xserver = {
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
    windowManager.default = "xmonad";
    desktopManager.xterm.enable = false;
    desktopManager.default = "none";
    libinput.enable = true;
    displayManager = {
      slim = {
	enable = true;
	defaultUser = "chenyu";
      };
    sessionCommands = with pkgs; lib.mkAfter
	''
	(sleep 3; xset r rate 200 40) &
	(sleep 2; stalonetray) &
	(sleep 3; xscreensaver -no-splash) &
	(sleep 3; xfce4-power-manager) &
	(sleep 1; xmodmap ~/.xmodmap) &
	'';
    };
    wacom.enable = true;
  };

  services.emacs.enable = true;
  services.emacs.defaultEditor = true;
  services.emacs.package = pkgs.emacs;
  # Define a user account. Don't forget to set a password with ‘passwd’.
  # users.users.guest = {
  #   isNormalUser = true;
  #   uid = 1000;
  # };
  users.defaultUserShell = pkgs.zsh;
  users.extraUsers.chenyu = { 
     isNormalUser = true ; 
     home = "/home/chenyu" ; 
     hashedPassword = "{CHENYU_PASSWD}" ; 
     extraGroups = ["wheel" "postgres" "networkmanager" "docker" "audio" "video" "adbusers"] ; 
  }; 
  security.sudo.enable = true;
  security.rngd.enable = true;
  fonts = { 
    fontconfig.enable = true; 
    enableFontDir = true; 
    enableGhostscriptFonts = true; 
    fonts = with pkgs; [ 
       noto-fonts 
       noto-fonts-cjk 
       noto-fonts-emoji 
       wqy_microhei 
       wqy_zenhei 
       source-code-pro
    ]; 
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "18.09"; # Did you read the comment?

}
