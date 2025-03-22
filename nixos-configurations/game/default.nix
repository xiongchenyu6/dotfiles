# Edit
{
  inputs,
  lib,
  ezModules,
  config,
  pkgs,
  mylib,
  shares,
  ...
}:
{
  imports = with inputs; [
    ./hardware-configuration.nix
    ezModules.root
    ezModules.dvorak
    ezModules."freeman.xiong"
    ezModules.misc
    ezModules.client-cli
    ezModules.gui
    ezModules.vr
    ezModules.core
    ezModules.greetd
    ezModules.dn42
    ezModules.bird-inner
    ezModules.virtualisation
    ezModules.falcon-sensor
    ezModules.wayland
    nixos-hardware.nixosModules.lenovo-legion-16ach6h
    srvos.nixosModules.desktop
    vscode-server.nixosModules.default
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
    {
      topology.self.interfaces.home = {
        type = "wireguard";
        addresses = [ "172.22.240.99/27" ];
      };
    }
  ];

  sops.secrets."wireguard/game" = { };

  system.nixos.tags = [
    "nvidia"
    "gui"
  ];

  powerManagement.cpuFreqGovernor = "performance";

  hardware = {
    enableRedistributableFirmware = true;
  };

  environment = {
    systemPackages = [
      pkgs.cloudflare-warp
    ];
  };

  boot = {

    binfmt.emulatedSystems = [ "aarch64-linux" ];
    initrd.kernelModules = [
      "vfio_pci"
      "vfio"
      "vfio_iommu_type1"
    ];
    kernelParams = [
      "iommu=pt"
    ];

    kernel = {
      sysctl = {
        "net.ipv4.ip_forward" = 1;
      };
    };

    loader = {
      systemd-boot = {
        configurationLimit = 12;
        enable = true;
      };
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot";
      };
    };
  };

  networking =
    let
      file-path = builtins.split "/" (toString ./.);
      hostName = lib.last file-path;
    in
    {
      inherit hostName;

      firewall = {
        enable = true;
        allowedTCPPorts = [
          89
          179
          5002
        ];
        allowedUDPPorts = [
          89
          179
          5353
          6696
          33434
        ];
        interfaces.wg_mail.allowedTCPPorts = [
          2222
          8080
        ];
        interfaces.wg_mail.allowedUDPPorts = [ 2222 ];
        interfaces.wt0.allowedTCPPorts = [ 2222 ];
        interfaces.wt0.allowedUDPPorts = [ 2222 ];
      };

      networkmanager = {
        enable = true;
        wifi = {
          powersave = true;
        };
      };
      enableIPv6 = true;
      wg-quick = {
        interfaces = {
          wg_mail = {
            privateKeyFile = config.sops.secrets."wireguard/game".path;
            table = "off";
            address = [ "fe80::102/64" ];
            postUp = ''
              ${pkgs.iproute2}/bin/ip addr add dev wg_mail 172.22.240.99/32 peer 172.22.240.96/27
              ${pkgs.iproute2}/bin/ip addr add dev wg_mail fd48:4b4:f3::3/128 peer fd48:4b4:f3::1/128
              ${pkgs.iproute2}/bin/ip link set multicast on dev wg_mail
            '';

            peers = [
              {
                endpoint = "43.156.66.157:22617";
                publicKey = shares.hosts-dict.mail.wg.public-key;
                persistentKeepalive = 30;
                allowedIPs = [
                  "10.0.0.0/8"
                  "172.20.0.0/14"
                  "172.31.0.0/16"
                  "fd00::/8"
                  "fe80::/10"
                  "fd48:4b4:f3::/48"
                  "ff02::1:6/128"
                  "224.0.0.251/32" # avahi
                  "ff02::fb/128" # avahi
                ];
              }
            ];
          };
        };
      };
      useDHCP = lib.mkDefault true;
    };

  services = {

    cloudflare-warp = {
      enable = true;
    };

    sunshine = {
      enable = true;
      openFirewall = true;
      capSysAdmin = true;
    };

    postgresql = {
      enable = true;
      package = pkgs.postgresql_17_jit;
      enableJIT = true;
      enableTCPIP = true;
      extensions =
        ps: with ps; [
          postgis
          pg_repack
          pg_cron
        ];
      settings = {
        log_connections = true;
        log_statement = "all";
        logging_collector = true;
        log_disconnections = true;
        log_destination = lib.mkForce "syslog";
        shared_preload_libraries = "pg_cron";
        "cron.database_name" = "postgres";
      };
      ensureUsers = [
        {
          name = "freeman.xiong";
          ensureDBOwnership = true;
          ensureClauses = {
            superuser = true;
          };
        }
      ];
      ensureDatabases = [ "freeman.xiong" ];
    };

    vscode-server.enable = true;
    frp = {
      enable = false;
      role = "client";
      settings = {
        serverAddr = "mail.autolife-robotics.me";
        serverPort = 7000;
        auth = {
          method = "token";
          token = builtins.readFile ../../../secrets/frp.token;
        };
      };
    };

    #v2raya.enable = true;
    netbird.enable = true;
    babeld = {
      interfaces = {
        wg_mail = {
          hello-interval = 5;
          split-horizon = "auto";
          type = "wired";
        };
      };
    };
    bird = {
      enable = true;
      config = mylib.bird2-inner-config "172.22.240.99" "fd48:4b4:f3::3";
    };
  };

  security.krb5.settings = {
    realms =
      let
        tronRealm = "TRONTECH.LINK";
        tronDomain = "trontech.link";
      in
      {
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

  programs = {
    adb = {
      enable = true;
    };
  };

  home-manager = {
    users = {
      "freeman.xiong" = {
        programs = {
          waybar = {
            settings = {
              network = {
                interface = "wlp4s0";
              };
            };
          };
        };
      };
    };
  };
}
