# Edit
{
  inputs,
  lib,
  ezModules,
  pkgs,
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
    ezModules.disk-recovery
    ezModules.core
    ezModules.wgmesh
    ezModules.greetd
    ezModules.tlp
    ezModules.falcon-sensor
    ezModules.wayland
    ezModules.openfortivpn-config
    lanzaboote.nixosModules.lanzaboote
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
    nixos-hardware.nixosModules.dell-latitude-5520
    {
      topology.self.interfaces.home = {
        type = "wireguard";
        addresses = [ "172.22.240.98/27" ];
      };
    }
  ];

  boot.kernelParams = [
    "mem_sleep_default=deep"
    "pcie_aspm=force"
    "nvme.noacpi=1"
    "i915.enable_psr=1"
  ];

  services.logind = {
    lidSwitch = "suspend";
    lidSwitchDocked = "ignore";
  };

  # Enable users/freeman gui
  system.nixos.tags = [
    "gui"
  ];

  zramSwap.enable = true;

  hardware = {
    enableRedistributableFirmware = true;
  };

  powerManagement.cpuFreqGovernor = "performance";

  boot = {
    kernelPackages = lib.mkForce pkgs.linuxPackages_latest;
    binfmt.emulatedSystems = [ "aarch64-linux" ];
    supportedFilesystems = [ "nfs4" ];

    kernel = {
      sysctl = {
        "net.ipv4.ip_forward" = 1;
      };
    };

    extraModprobeConfig = ''
      options snd-intel-dspcfg dsp_driver=1
    '';
    lanzaboote = {
      enable = true;
      pkiBundle = "/var/lib/sbctl";
    };

    loader = {
      systemd-boot.enable = lib.mkForce false;
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
          8000
          8080
        ];
        allowedUDPPorts = [
          89
          179
          5353
          6696
          33434
        ];
        interfaces.wgmesh.allowedTCPPorts = [ 2222 ];
        interfaces.wgmesh.allowedUDPPorts = [ 2222 ];
      };

      networkmanager = {
        enable = true;
        dns = "systemd-resolved";
        wifi = {
          powersave = true;
        };
        ethernet = {
          macAddress = "random";
        };
      };
      enableIPv6 = true;

      useDHCP = lib.mkDefault true;

    };

  services = {
    # cloudflare-warp = {
    #   enable = true;
    # };

    # kanidm = {
    #   enableClient = true;
    #   clientSettings = {
    #     uri = "https://kanidm.${config.networking.domain}";
    #   };
    #   enablePam = true;
    #   unixSettings = {
    #     default_shell = "${pkgs.bashInteractive}/bin/bash";
    #     home_alias = "name";
    #     home_attr = "uuid";
    #     home_prefix = "/mnt/disk1/";
    #     pam_allowed_login_groups = [ "admin" ];
    #   };
    # };

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
        "cron.database_name" = "api";
        "cron.use_background_workers" = "on";
        max_worker_processes = 20;
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

    netbird.enable = true;

    geth = {
      test-beacon = {
        enable = false;
        syncmode = "light";
        network = "goerli";
        http = {
          enable = true;
          apis = [
            "eth"
            "net"
            "web3"
            "debug"
          ];
        };
      };
    };
  };

  home-manager = {
    users = {
      "freeman.xiong" = {
        programs = {
          waybar = {
            settings = {
              network = {
                interface = "wlp0s20f3";
              };
            };
          };
        };
      };
    };
  };
}
