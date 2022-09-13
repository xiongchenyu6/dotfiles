# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, options, lib, ... }:
let
  secret = (import ./secret.nix { inherit lib; });
  script = (import ../dn42/update-roa.nix { inherit pkgs; });
in
rec {
  imports = [
    # Include the results of the hardware scan.
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
    kernel = {
      sysctl = {
        # The Magic SysRq key is a key combo that allows users connected to the
        # system console of a Linux kernel to perform some low-level commands.
        # Disable it, since we don't need it, and is a potential security concern.
        "kernel.sysrq" = 511;
        "net.ipv4.ip_forward" = 1;
        "net.ipv6.conf.all.forwarding" = 1;
        "net.ipv6.conf.default.forwarding" = 1;

        "net.ipv4.conf.default.rp_filter" = 0;
        "net.ipv4.conf.all.rp_filter" = 0;

        # ## TCP hardening
        # # Prevent bogus ICMP errors from filling up logs.
        # "net.ipv4.icmp_ignore_bogus_error_responses" = 1;
        # # Reverse path filtering causes the kernel to do source validation of
        # # packets received from all interfaces. This can mitigate IP spoofing.
        # "net.ipv4.conf.default.rp_filter" = 1;
        # "net.ipv4.conf.all.rp_filter" = 1;
        # # Do not accept IP source route packets (we're not a router)
        # "net.ipv4.conf.all.accept_source_route" = 0;
        # "net.ipv6.conf.all.accept_source_route" = 0;
        # # Don't send ICMP redirects (again, we're on a router)
        # "net.ipv4.conf.all.send_redirects" = 0;
        # "net.ipv4.conf.default.send_redirects" = 0;
        # # Refuse ICMP redirects (MITM mitigations)
        # "net.ipv4.conf.all.accept_redirects" = 0;
        # "net.ipv4.conf.default.accept_redirects" = 0;
        # "net.ipv4.conf.all.secure_redirects" = 0;
        # "net.ipv4.conf.default.secure_redirects" = 0;
        # "net.ipv6.conf.all.accept_redirects" = 0;
        # "net.ipv6.conf.default.accept_redirects" = 0;

        # # Protects against SYN flood attacks
        "net.ipv4.tcp_syncookies" = 1;
        # # Incomplete protection again TIME-WAIT assassination
        "net.ipv4.tcp_rfc1337" = 1;

        # ## TCP optimization
        # # TCP Fast Open is a TCP extension that reduces network latency by packing
        # # data in the sender’s initial TCP SYN. Setting 3 = enable TCP Fast Open for
        # # both incoming and outgoing connections:
        "net.ipv4.tcp_fastopen" = 3;
        # # Bufferbloat mitigations + slight improvement in throughput & latency
        "net.ipv4.tcp_congestion_control" = "bbr";
        "net.core.default_qdisc" = "cake";
      };
    };
  };

  networking = {
    firewall = {
      allowedTCPPorts = [ 179 ];
      allowedUDPPorts = [ 179 33434 ];
      enable = true;
    };

    networkmanager = { enable = true; };
    enableIPv6 = true;
    hostName = "nixos"; # Define your hostname.
    # Configure network proxy if necessary
    # networking.proxy.default = "http://user:password@proxy:port/";
    # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

    # Enable networking
    #networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
    wg-quick = {
      interfaces = {
        wg_freeman = {
          privateKey = secret.freeman.wg.private-key;
          address = [ "172.22.240.98/27" "fe80::101/64" "fd48:4b4:f3::2/48" ];
          dns = [ "fe80::100%wg_freeman" "172.22.240.97" "1.1.1.1" ];
          peers = [{
            endpoint = "freeman.engineer:22616";
            publicKey = secret.my.wg.public-key;
            persistentKeepalive = 30;
            allowedIPs = [
              "10.0.0.0/8"
              "172.20.0.0/14"
              "172.31.0.0/16"
              "fd00::/8"
              "fe80::/10"
              "fd48:4b4:f3::/48"
            ];
          }];
        };
      };
    };
  };

  virtualisation = {
    docker = {
      enable = true;
      rootless = {
        enable = true;
      };
    };
  };

  # Set your time zone.
  time = { timeZone = "Asia/Singapore"; };

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
    bird2 = {
      enable = true;
      checkConfig = false;
      config = ''
        ################################################
        #               Variable header                #
        ################################################

        define OWNAS =  4242422616;
        define OWNIP =  172.22.240.98;
        define OWNIPv6 = fd48:4b4:f3::2;
        define OWNNET = 172.22.240.96/27;
        define OWNNETv6 = fd48:4b4:f3::/48;
        define OWNNETSET = [172.22.240.96/27+];
        define OWNNETSETv6 = [fd48:4b4:f3::/48+];

        ################################################
        #                 Header end                   #
        ################################################

        router id OWNIP;

        protocol device {
            scan time 10;
        }

        /*
         *  Utility functions
         */

        function is_self_net() {
          return net ~ OWNNETSET;
        }

        function is_self_net_v6() {
          return net ~ OWNNETSETv6;
        }

        function is_valid_network() {
          return net ~ [
            172.20.0.0/14{21,29}, # dn42
            172.20.0.0/24{28,32}, # dn42 Anycast
            172.21.0.0/24{28,32}, # dn42 Anycast
            172.22.0.0/24{28,32}, # dn42 Anycast
            172.23.0.0/24{28,32}, # dn42 Anycast
            172.31.0.0/16+,       # ChaosVPN
            10.100.0.0/14+,       # ChaosVPN
            10.127.0.0/16{16,32}, # neonetwork
            10.0.0.0/8{15,24}     # Freifunk.net
          ];
        }

        roa4 table dn42_roa;
        roa6 table dn42_roa_v6;

        function is_valid_network_v6() {
          return net ~ [
            fd00::/8{44,64} # ULA address space as per RFC 4193
          ];
        }

        protocol kernel {
            scan time 20;

            ipv6 {
                import none;
                export filter {
                    if source = RTS_STATIC then reject;
                    krt_prefsrc = OWNIPv6;
                    accept;
                };
            };
        };

        protocol kernel {
            scan time 20;

            ipv4 {
                import none;
                export filter {
                    if source = RTS_STATIC then reject;
                    krt_prefsrc = OWNIP;
                    accept;
                };
            };
        }

        protocol static {
            route OWNNET reject;

            ipv4 {
                import all;
                export none;
            };
        }

        protocol static {
            route OWNNETv6 reject;

            ipv6 {
                import all;
                export none;
            };
        }

        template bgp dnpeers {
            local as OWNAS;
            path metric 1;

            ipv4 {
                import filter {
                  if is_valid_network() && !is_self_net() then {
                    if (roa_check(dn42_roa, net, bgp_path.last) != ROA_VALID) then {
                      print "[dn42] ROA check failed for ", net, " ASN ", bgp_path.last;
                      reject;
                    } else accept;
                  } else reject;
                };

                export filter { if is_valid_network() && source ~ [RTS_STATIC, RTS_BGP] then accept; else reject; };
                import limit 1000 action block;
            };

            ipv6 {   
                import filter {
                  if is_valid_network_v6() && !is_self_net_v6() then {
                    if (roa_check(dn42_roa_v6, net, bgp_path.last) != ROA_VALID) then {
                      print "[dn42] ROA check failed for ", net, " ASN ", bgp_path.last;
                      reject;
                    } else accept;
                  } else reject;
                };
                export filter { if is_valid_network_v6() && source ~ [RTS_STATIC, RTS_BGP] then accept; else reject; };
                import limit 1000 action block; 
            };
        }
        protocol static {
            roa4 { table dn42_roa; };
            include "/etc/bird/roa_dn42.conf";
        };

        protocol static {
            roa6 { table dn42_roa_v6; };
            include "/etc/bird/roa_dn42_v6.conf";
        };

        protocol bgp ibgp_my  {

          local as OWNAS;
          neighbor fd48:4b4:f3::1 as OWNAS;
          direct;

          ipv4 {
              next hop self;
              # Optional cost, e.g. based off latency
              cost 50;

              import all;
              export all;
          };

          ipv6 {
              next hop self;
              cost 50;

              import all;
              export all;
          };
        }

      '';
    };

    cachix-agent = {
      enable = true;
      credentialsFile = ./cachix.secret;
      verbose = true;
    };

    xserver = {
      enable = true;
      layout = "us";
      displayManager = {
        lightdm = { enable = true; };
        autoLogin = {
          enable = true;
          user = "freeman";
        };
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
      libinput = { enable = true; };
      # Enable automatic login for the user.
    };

    aria2 = { enable = true; };

    # Enable CUPS to print documents.
    printing = {
      enable = true;
    };

    gnome = { gnome-keyring = { enable = true; }; };

    upower = { enable = true; };

    dbus = { enable = true; };
    # Enable the OpenSSH daemon.
    openssh = { enable = true; };
    pipewire = {
      enable = true;
      alsa = {
        enable = true;
        support32Bit = true;
      };
      pulse = { enable = true; };
      # If you want to use JACK applications, uncomment this
      #jack.enable = true;

      # use the example session manager (no others are packaged yet so this is enabled by default,
      # no need to redefine it in your config for now)
      #media-session.enable = true;
    };

    blueman = { enable = true; };

    udev = {
      extraRules = ''
        ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", MODE="0666", RUN+="${pkgs.coreutils}/bin/chmod a+w /sys/class/backlight/%k/brightness"
      '';
    };

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
    pulseaudio = { enable = false; };
    bluetooth = { enable = true; };
  };

  systemd = {
    timers = {
      dn42-roa = {
        description = "Trigger a ROA table update";

        timerConfig = {
          OnBootSec = "5m";
          OnUnitInactiveSec = "1h";
          Unit = "dn42-roa.service";
        };

        wantedBy = [ "timers.target" ];
        before = [ "bird2.service" ];
      };
    };
    services = {
      dn42-roa = {
        after = [ "network.target" ];
        description = "DN42 ROA Updated";
        unitConfig = { Type = "one-shot"; };
        serviceConfig = { ExecStart = "${script}/bin/update-roa"; };
      };
      upower = { enable = true; };
    };

  };

  # Enable sound with pipewire.
  sound = { enable = true; };

  security = {
    rtkit = { enable = true; };
    sudo = { enable = true; };
    acme = { acceptTerms = true; };
    pki.certificates = [
      ''
        -----BEGIN CERTIFICATE-----
        MIID8DCCAtigAwIBAgIFIBYBAAAwDQYJKoZIhvcNAQELBQAwYjELMAkGA1UEBhMC
        WEQxDTALBgNVBAoMBGRuNDIxIzAhBgNVBAsMGmRuNDIgQ2VydGlmaWNhdGUgQXV0
        aG9yaXR5MR8wHQYDVQQDDBZkbjQyIFJvb3QgQXV0aG9yaXR5IENBMCAXDTE2MDEx
        NjAwMTIwNFoYDzIwMzAxMjMxMjM1OTU5WjBiMQswCQYDVQQGEwJYRDENMAsGA1UE
        CgwEZG40MjEjMCEGA1UECwwaZG40MiBDZXJ0aWZpY2F0ZSBBdXRob3JpdHkxHzAd
        BgNVBAMMFmRuNDIgUm9vdCBBdXRob3JpdHkgQ0EwggEiMA0GCSqGSIb3DQEBAQUA
        A4IBDwAwggEKAoIBAQDBGRDeAYYR8YIMsNTl/5rI46r0AAiCwM9/BXohl8G1i6PR
        VO76BA931VyYS9mIGMEXEJLlJPrvYetdexHlvrqJ8mDJO4IFOnRUYCNmGtjNKHvx
        6lUlmowEoP+dSFRMnbwtoN9xrmRHDed1BfTFAirSDL6jY1RiK60p62oIpF6o6/FS
        FE7RXUEv0xm65II2etGj8oT2B7L2DDDb23bu6RQFx491tz/V1TVW0JJE3yYeAPqu
        y3rJUGddafj5/SWnHdtAsUK8RVfhyRxCummAHuolmRKfbyOj0i5KzRXkfEn50cDw
        GQwVUM6mUbuqFrKC7PRhRIwc3WVgBHewTZlnF/sJAgMBAAGjgaowgacwDgYDVR0P
        AQH/BAQDAgEGMA8GA1UdEwEB/wQFMAMBAf8wHQYDVR0OBBYEFFR2iLLAtTDQ/E/J
        bTv5jFURrBUVMB8GA1UdIwQYMBaAFFR2iLLAtTDQ/E/JbTv5jFURrBUVMEQGA1Ud
        HgQ9MDugOTAHggUuZG40MjAKhwisFAAA//wAADAihyD9QgAAAAAAAAAAAAAAAAAA
        //8AAAAAAAAAAAAAAAAAADANBgkqhkiG9w0BAQsFAAOCAQEAXKQ7QaCBaeJxmU11
        S1ogDSrZ7Oq8jU+wbPMuQRqgdfPefjrgp7nbzfUW5GrL58wqj+5/FAqltflmSIHl
        aB4MpqM8pyvjlc/jYxUNFglj2WYxO0IufBrlKI5ePZ4omUjpR4YR4gQpYCuWlZmu
        P6v/P0WrfgdFTk0LGEA9OwKcTqkPpcI/SjB3rmZcs42yQWvimAF94GtScE09uKlI
        9QLS2UBmtl5EJRFVrDEC12dyamq8dDRfddyaT4MoQOAq3D9BQ1pHByu3pz/QFaJC
        1zAi8vbktPY7OMprTOc8pHDL3q8KFP8jJcoEzZ5Jw0vkCrULhLXvtFtjB0djzVxQ
        C0IKqQ==
        -----END CERTIFICATE-----
      ''
    ];
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
          "docker"
          "socket"
          "spi"
          "bus"
          "dropbox"
        ];
        packages = with pkgs; [ tdesktop ];
      };
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.

  # Allow unfree packages
  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = true;
    };
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment = {
    systemPackages = with pkgs; [
      automake
      asciinema
      awscli2
      clang
      conky
      cabal2nix
      cachix
      consul
      discord
      dig
      lua
      nixopsUnstable
      neofetch
      exa
      feh
      fd
      procs
      tealdeer
      socat
      rustscan
      gitAndTools.gitflow
      git-crypt
      geoip
      gnumake
      gh
      haskell-language-server
      (haskellPackages.ghcWithPackages (self:
        with haskellPackages;
        with pkgs.haskell.lib; [
          apply-refact
          cabal-install
          hlint
          stylish-haskell
          hasktags
          hoogle
          pandoc
        ]))
      heroku
      imagemagick
      ispell
      inetutils
      killall
      lsof
      libxml2
      libtool
      libsodium
      (python3.withPackages (ps: [ myRepo.my_cookies ]))
      pinentry
      linuxPackages.ply
      wakatime
      #myRepo.example-package
      node2nix
      nodejs
      nodejs-16_x
      nodePackages."typescript-language-server"
      nodePackages."bash-language-server"
      openssl
      # openjdk
      pkgconfig
      protobuf
      plantuml
      ripgrep
      rnix-lsp
      tree
      tomb
      tcpdump
      unzip
      vlc
      scrot
      stow
      slack
      wineWowPackages.staging
      wireguard-tools
      wireshark
      #wpa_supplicant_gui
      wakatime
      whatsapp-for-linux
      xclip
      xddxdd.qq
      xddxdd.wechat-uos
      xscreensaver
      zoom-us
    ];
    pathsToLink = [ "/share/zsh" ];
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
    nm-applet = { enable = true; };
  };

  # List services that you want to enable:

  fonts = {
    fontconfig = { enable = true; };
    fontDir = { enable = true; };
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      wqy_microhei
      wqy_zenhei
      (nerdfonts.override { fonts = [ "Hack" ]; })
      jetbrains-mono
    ];
  };
  nix = {
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
    settings = {
      experimental-features = [ "nix-command" "flakes" ];
      trusted-users = [ "root" "freeman" ];
    };
  };

  # Open ports in the firewall.
  # Or disable the firewall altogether.

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system = { stateVersion = "22.11"; }; # Did you read the comment?
}
