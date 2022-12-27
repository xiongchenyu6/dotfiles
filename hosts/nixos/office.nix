# Edit
{ config, lib, modulesPath, suites, profiles, ... }: rec {
  imports = [
    # Include the results of the hardware scan.
    (modulesPath + "/installer/scan/not-detected.nix")
    profiles.server-apps.mysql
    profiles.core.nixos
    profiles.client-pkgs.nixos
    profiles.users.root.nixos
    profiles.users."freeman.xiong"
  ] ++ suites.client-base;

  sops.secrets."wireguard/office" =
    builtins.trace (lib.mine.bird2-inner-config "a" "b") { };

  system.nixos.tags = [ "with-gui" ];

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/799ba8ac-87bb-4c4e-b060-1787b4708a90";
      fsType = "ext4";
    };

    "/boot/efi" = {
      device = "/dev/disk/by-uuid/209B-184A";
      fsType = "vfat";
    };

    "/mnt/hydra" = {
      device = "hydra.inner.trontech.link:/export";
      fsType = "nfs";
      options = [ "x-systemd.automount" "noauto" ];
    };
  };

  swapDevices = [ ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  # networking.interfaces.wlp0s20f3.useDHCP = lib.mkDefault true;

  powerManagement = { cpuFreqGovernor = lib.mkDefault "powersave"; };

  hardware = {
    cpu = {
      intel = {
        updateMicrocode =
          lib.mkDefault config.hardware.enableRedistributableFirmware;
      };
    };
  };

  nix = {
    distributedBuilds = false;
    buildMachines = [{
      hostName = "hydra.inner.trontech.link";
      sshUser = "freeman.xiong";
      systems = [ "x86_64-linux" ];
      maxJobs = 2;
    }];
  };

  nixpkgs.config.permittedInsecurePackages = [ "python3.10-certifi-2022.12.7" ];

  boot = {
    initrd = {
      availableKernelModules =
        [ "xhci_pci" "thunderbolt" "nvme" "usb_storage" "usbhid" "sd_mod" ];
      kernelModules = [ ];
    };

    kernelModules = [ "kvm-intel" "hid-nintendo" "v4l2loopback" ];

    extraModulePackages = with config.boot.kernelPackages; [ v4l2loopback.out ];

    extraModprobeConfig = ''
      options i915 force_probe=46a6
      options snd-intel-dspcfg dsp_driver=1
    '';

    tmpOnTmpfs = lib.mkDefault true;

    loader = {
      systemd-boot = { editor = false; };
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
      allowedTCPPorts = [ 179 ];
      allowedUDPPorts = [ 179 33434 ];
      enable = false;
    };

    networkmanager = { enable = true; };
    enableIPv6 = true;
    #hostName = "office"; # Define your hostname.
    # Enable networking
    #networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
    wg-quick = {
      interfaces = {
        wg_office = {
          privateKeyFile = config.sops.secrets."wireguard/office".path;
          address = [ "172.22.240.98/27" "fe80::101/64" "fd48:4b4:f3::2/48" ];
          dns = [ "fe80::100%wg_office" "172.22.240.97" "1.1.1.1" ];
          peers = [{
            endpoint = "freeman.engineer:22616";
            publicKey = profiles.share.hosts-dict.mail.wg.pk;
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
        wg_tronlink = {
          privateKeyFile = config.sops.secrets."wireguard/office".path;
          address = [ "172.64.224.2/24" "fe80::102/64" ];
          peers = [{
            endpoint = "vpn.trontech.link:22617";
            publicKey = profiles.share.hosts-dict.tronlink.wg.pk;
            persistentKeepalive = 5;
            allowedIPs = [
              "172.64.224.1/24"
              "fe80::101/64"
              "172.32.0.0/16"
              "18.218.96.133/32"
            ];
          }];
        };
      };
    };
    extraHosts = ''
      #  127.0.0.1 freeman.engineer
    '';

    useDHCP = lib.mkDefault true;

    nftables = {
      enable = true;
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
            udp dport 179 accept
            udp dport 33434 accept

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
    nat = {
      enable = true;
      internalInterfaces = [ "ve-+" ];
      externalInterface = "wlp0s20f3";
      # Lazy IPv6 connectivity for the container
      enableIPv6 = true;
    };
  };
  services = {
    bird2 = {
      config = lib.mine.bird2-inner-config "172.22.240.98" "fd48:4b4:f3::2";
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
