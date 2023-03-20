# Edit
{ config, lib, suites, profiles, pkgs, ... }: {
  imports = [
    ./hardware-configuration.nix
    profiles.server-apps.mysql
    profiles.core.nixos
    profiles.client-pkgs.nixos
    profiles.users.root.nixos
    profiles.dvorak
    profiles.users."freeman.xiong"
    profiles.hardwares.misc
  ] ++ suites.client-base ++ suites.client-network;

  sops.secrets."wireguard/office" = { };

  # Enable users/freeman gui
  system.nixos.tags = [ "with-gui" ];

  fileSystems = {
    "/mnt/hydra" = {
      device = "hydra.inner.trontech.link:/export";
      fsType = "nfs";
      options = [ "x-systemd.automount" "noauto" ];
    };
  };

  hardware = { enableRedistributableFirmware = true; };

  nix = {
    distributedBuilds = false;
    buildMachines = [{
      hostName = "hydra.inner.trontech.link";
      sshUser = "freeman.xiong";
      systems = [ "x86_64-linux" ];
      sshKey = "/home/freeman.xiong/.ssh/id_ed25519";
      maxJobs = 2;
    }];
  };

  nixpkgs = {
    config = {
      permittedInsecurePackages = [ "electron-19.0.7" ];
      allowBroken = true;
    };
  };

  boot = {
    binfmt.emulatedSystems = [ "aarch64-linux" ];
    # kernel.sysctl."net.core.rmem_max" = 2500000;
    supportedFilesystems = [ "nfs4" ];
    kernelModules = [ "hid-nintendo" "v4l2loopback" "dummy" ];
    initrd.availableKernelModules = [ "sd_mod" ];

    extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback.out ];
    kernel = {
      sysctl = {
        "net.ipv4.ip_forward" = 1;
        # "net.ipv4.conf.default.rp_filter" = 0;
        # "net.ipv4.conf.all.rp_filter" = 0;
        # "net.ipv4.conf.default.forwarding" = 1;
        # "net.ipv4.conf.all.forwarding" = 1;

        # "net.ipv6.conf.all.accept_redirects" = 0;
        # "net.ipv6.conf.default.forwarding" = 1;
        # "net.ipv6.conf.all.forwarding" = 1;
      };
    };

    extraModprobeConfig = ''
      options snd-intel-dspcfg dsp_driver=1
      options kvm_intel nested=1
    '';

    tmpOnTmpfs = lib.mkDefault true;

    loader = {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/efi";
      };
      grub = {
        enable = true;
        efiSupport = true;
        version = 2;
        device = "nodev";
        configurationLimit = 5;
        useOSProber = true;
      };
      grub2-theme = {
        enable = true;
        icon = "white";
        theme = "whitesur";
        screen = "1080p";
        splashImage = ./grub.jpg;
        footer = true;
      };
    };
  };

  networking = {
    firewall = {
      allowedTCPPorts = [ 89 179 ];
      allowedUDPPorts = [ 89 179 6696 33434 ];
      enable = true;
    };

    networkmanager = {
      enable = true;
      # firewallBackend = "nftables";
      wifi = {
        powersave = true;
        macAddress = "random";
      };
      ethernet = { macAddress = "random"; };
      enableFccUnlock = true;
    };
    enableIPv6 = true;
    #hostName = "office"; # Define your hostname.
    # Enable networking
    #networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
    # wg-quick = with builtins;
    #   with lib; {
    #     interfaces = let
    #       generated_wg_config = listToAttrs (map (network:
    #         nameValuePair "wg_${network.name}" {
    #           privateKeyFile = config.sops.secrets."wireguard/office".path;
    #           inherit (network.assignedIPs-dict."${config.networking.hostName}")
    #             address;
    #           peers = (map (host: {
    #             endpoint = "${host.endpoint}:${toString host.wg.port}";
    #             publicKey =
    #               profiles.share.hosts-dict."${host.host}".wg.public-key;
    #             inherit (network) allowedIPs;
    #             persistentKeepalive = 30;
    #           }) (filter (x: (x.network == network.name) && (x.role == "vpn"))
    #             profiles.share.hosts));
    #         }) profiles.share.networks);
    #     in generated_wg_config;
    #     # dns = [ # "fe80::100%wg_office"
    #     #   # "172.22.240.97"
    #     #   "1.1.1.1"
    #     # ];
    #   };

    wg-quick = let
      privateKeyFile = config.sops.secrets."wireguard/office".path;
      table = "off";
    in {
      interfaces = {
        wg_mail = {
          inherit privateKeyFile table;
          # address = [ "fd48:4b4:f3::2" ];
          address = [ "fe80::101/64" ];
          postUp = ''
            ${pkgs.iproute2}/bin/ip addr add dev wg_mail 172.22.240.98 peer 172.22.240.97
            # ${pkgs.iproute2}/bin/ip addr add dev wg_mail fe80::101 peer fd48:4b4:f3::1
          '';
          # dns = [ # "fe80::100%wg_office"
          #   # "172.22.240.97"
          #   "1.1.1.1"
          # ];
          peers = [{
            endpoint = "mail.freeman.engineer:22616";
            publicKey = profiles.share.hosts-dict.mail.wg.public-key;
            persistentKeepalive = 30;
            allowedIPs = [
              # "::/0"
              # "0.0.0.0/0"
              "10.0.0.0/8"
              "172.20.0.0/14"
              "172.31.0.0/16"
              "fd00::/8"
              "fe80::/10"
              "fd48:4b4:f3::/48"
              "ff02::1:6/128"
            ];
          }];
        };
        # wg_tronlink = {
        #   inherit privateKeyFile;
        #   address = [ "172.64.224.3/24" "fe80::103/64" ];
        #   peers = [{
        #     endpoint = "vpn.trontech.link:22617";
        #     publicKey = profiles.share.hosts-dict.tronlink.wg.public-key;
        #     persistentKeepalive = 5;
        #     allowedIPs = [
        #       "172.64.224.1/24"
        #       "fe80::101/64"
        #       "172.32.0.0/16"
        #       "18.218.96.133/32"
        #       "13.212.2.33"
        #     ];
        #   }];
        # };
      };
    };
    useDHCP = lib.mkDefault true;

    nftables = {
      # enable = true;
      ruleset = ''
        # Check out https://wiki.nftables.org/ for better documentation.
        # Table for both IPv4 and IPv6.
        table inet filter {
        # Block all incomming connections traffic except SSH and "ping".
        chain input {
        type filter hook input priority 0;

        # accept any localhost traffic
        iifname lo accept

        # accept traffic originated from us
        ct state {established, related} accept

        # ICMP
        # routers may also want: mld-listener-query, nd-router-solicit
        ip6 nexthdr icmpv6 icmpv6 type { destination-unreachable, packet-too-big, time-exceeded, parameter-problem, nd-router-advert, nd-neighbor-solicit, nd-neighbor-advert } accept
        ip protocol icmp icmp type { destination-unreachable, router-advertisement, time-exceeded, parameter-problem } accept

        # allow "ping"
        ip6 nexthdr icmpv6 icmpv6 type echo-request accept
        ip protocol icmp icmp type echo-request accept

        # accept SSH connections (required for a server)
        tcp dport 22 accept
        tcp dport 179 accept
        tcp dport 8000 accept
        tcp dport 6788 accept
        udp dport 179 accept
        udp dport 33434 accept
        udp dport 6788 accept

        # count and drop any other traffic
        counter drop
        }

        # Allow all outgoing connections.
        chain output {
        type filter hook output priority 0;
        accept
        }

        chain forward {
        type filter hook forward priority 0;
        accept
        }
        }
      '';
    };
    # nat = {
    #   enable = true;
    #   internalInterfaces = [ "vie-+" ];
    #   externalInterface = "wlp0s20f3";
    #   # Lazy IPv6 connectivity for the container
    #   enableIPv6 = true;
    # };
  };

  services = {

    netbird.enable = true;

    babeld.enable = false;
    babeld.interfaces = {
      wg_mail = {
        hello-interval = 5;
        split-horizon = "auto";
        type = "wired";
      };
    };

    dgraph = { enable = false; };

    postgresql = {
      enable = true;
      authentication = ''
        local all all trust
      '';
    };

    chainlink = {
      enable = false;
      apicredentialsFilePath = ./chainlink/apicredentials;
      configFilePath = ./chainlink/core.toml;
      secretsFilePath = ./chainlink/secrets.toml;
    };

    java-tron = {
      enable = false;
      event-plugin = "mongodb";
    };

    mongodb = {
      enable = false;
      bind_ip = "0.0.0.0";
      enableAuth = true;
      package = pkgs.my-mongodb;
      initialRootPassword = "admin";
      # initialScript = initialScript;
      extraConfig = ''
        net:
          port: 27017
        systemLog:
          logAppend: true
        storage:
          wiredTiger:
             engineConfig:
                cacheSizeGB: 2
      '';
    };

    nginx = { enable = false; };

    # vikunja = {
    #   enable = true;
    #   setupNginx = true;
    #   frontendScheme = "http";
    #   frontendHostname = "localhost";
    # };
    bird2 = {
      enable = false;
      config = lib.mine.bird2-inner-config "172.22.240.98" "fd48:4b4:f3::2";
    };

    geth = {
      test-beacon = {
        enable = false;
        syncmode = "light";
        network = "goerli";
        # extraArgs = [ "--execution-endpoint" ];
        http = {
          enable = true;
          apis = [ "eth" "net" "web3" "debug" ];
        };
      };
    };
  };

  krb5 = {
    realms = let
      tronRealm = "TRONTECH.LINK";
      tronDomain = "trontech.link";
    in {
      "${tronRealm}" = {
        admin_server = "admin.inner.${tronDomain}";
        kdc = [ "admin.inner.${tronDomain}" ];
        default_domain = "admin.inner.${tronDomain}";
        kpasswd_server = "admin.inner.${tronDomain}";
        database_module = "openldap_ldapconf";
      };
      domain_realm = {
        "${tronDomain}" = tronRealm;
        ".inner.${tronDomain}" = tronRealm;
        ".${tronDomain}" = tronRealm;
      };
    };
  };

  home-manager = {
    users = {
      "freeman.xiong" = {
        sops = {
          gnupg = { home = "~/.gnupg"; };
          secrets = {
            # The path to the file to decrypt.
            gptcommit = {
              name = "gptcommit";
              path = "/home/freeman.xiong/.config/gptcommit/config.toml";
              mode = "777";
            };
          };
        };

        services = {
          git-sync = {
            enable = true;
            repositories = {
              notes = {
                path = "/home/freeman.xiong/Private/xiongchenyu6.github.io";
                uri = "git@github.com:xiongchenyu6/xiongchenyu6.github.io.git";
                # interval = 10;
              };
            };
          };
        };
        programs = {
          waybar = { settings = { network = { interface = "wlp0s20f3"; }; }; };
        };
      };
    };
  };

  # containers.nextcloud = {
  #   autoStart = true;
  #   privateNetwork = true;
  #   hostAddress = "192.168.100.10";
  #   localAddress = "192.168.100.11";
  #   hostAddress6 = "fc00::1";
  #   localAddress6 = "fc00::2";
  #   config = { config, pkgs, ... }: {

  #     services.nextcloud = {
  #       enable = true;
  #       hostName = "localhost";
  #       config = {
  #         adminpassFile = toString (pkgs.writeText "adminpass"
  #           "test123"); # DON'T DO THIS IN PRODUCTION - the password file will be world-readable in the Nix Store!
  #         extraTrustedDomains = [ "192.168.100.11" ];
  #       };
  #     };

  #     system.stateVersion = "22.11";

  #     networking.firewall = {
  #       enable = true;
  #       allowedTCPPorts = [ 80 ];
  #     };
  #     # Manually configure nameserver. Using resolved inside the container seems to fail
  #     # currently
  #     environment.etc."resolv.conf".text = "nameserver 8.8.8.8";
  #   };
  # };
}
