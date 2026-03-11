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
    # ezModules.falcon-sensor  # Disabled - broken
    ezModules.wayland
    ezModules.openfortivpn-config
    lanzaboote.nixosModules.lanzaboote
    nixos-hardware.nixosModules.lenovo-legion-16ach6h
    srvos.nixosModules.desktop
    srvos.nixosModules.mixins-tracing
    # Import Hashtopolis agent module from NUR packages
    xiongchenyu6.nixosModules.hashtopolis-agent
    {
      topology.self.interfaces.home = {
        type = "wireguard";
        addresses = [ "172.22.240.99/27" ];
      };
    }
    #./dnf-native.nix
    ./hashtopolis-agent.nix
    # ./vast-cli.nix  # Moved to home-manager module
  ];

  sops.secrets."wireguard/game" = { };

  system.nixos.tags = [
    "nvidia"
    "gui"
  ];

  powerManagement.cpuFreqGovernor = "performance";

  hardware = {
    enableRedistributableFirmware = true;
    nvidia-container-toolkit.enable = true;
  };

  environment = {
    systemPackages = with pkgs; [
      cloudflare-warp
      android-tools # Replaces programs.adb
      inputs.lazynixos.packages.${pkgs.system}.default
    ];
  };
  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.beta;
  boot = {

    binfmt.emulatedSystems = [ "aarch64-linux" ];
    initrd.kernelModules = [
      "vfio_pci"
      "vfio"
      "vfio_iommu_type1"
    ];
    kernelModules = [ "sp5100_tco" ]; # AMD watchdog module for Legion 16ach6h
    kernelParams = [
      "iommu=pt"
      "xhci_hcd.quirks=270336"
      "usbcore.autosuspend=-1"
      #"usbcore.old_scheme_first=1"
      "sp5100_tco.nowayout=1" # Prevent watchdog from being disabled
    ];

    # extraModulePackages = with pkgs; [
    #   linuxKernel.packages.linux_6_14.cryptodev
    # ];

    kernel = {
      sysctl = {
        "net.ipv4.ip_forward" = 1;
      };
    };

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

  systemd.services.ModemManager.enable = false;

  # Disable NVIDIA power daemon (fails when GPU is bound to vfio/power management unsupported)
  systemd.services.nvidia-powerd.enable = false;

  # Hardware watchdog configuration using new systemd options
  systemd.settings.Manager = {
    RuntimeWatchdogSec = "30s"; # Reboot if system hangs for 30 seconds
    RebootWatchdogSec = "10min"; # Allow 10 minutes for reboot to complete
    KExecWatchdogSec = "1min"; # Time for kexec reboot
  };

  # Enable watchdog daemon
  services.watchdogd = {
    enable = true;
    settings = {
      "device /dev/watchdog" = {
        timeout = 30; # Hardware watchdog timeout in seconds
        interval = 10; # Ping interval in seconds
        safe-exit = true; # Disable watchdog on clean exit
      };
      loadavg = {
        enabled = true;
        interval = 60;
        warning = 8.0; # Warning at load average 8
        critical = 12.0; # Critical at load average 12 (will trigger reboot)
      };
      meminfo = {
        enabled = true;
        interval = 60;
        warning = 0.85; # Warning at 85% memory usage
        critical = 0.95; # Critical at 95% memory usage (will trigger reboot)
      };
      filenr = {
        enabled = true;
        logmark = true;
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
      nameservers = [ "1.1.1.1" ];

      firewall = {
        enable = true;

        allowedTCPPorts = [
          89
          179
          5002
          6112
          6113
          6114
          6115
          6116
          6117
          6118
          6119
        ];
        allowedUDPPorts = [
          89
          179
          5353
          6112
          6113
          6114
          6115
          6116
          6117
          6118
          6119
          6696
          33434
        ];
        trustedInterfaces = [
          "virbr0"
          "virbr10"
        ]; # for libvirt
        interfaces.wg_tcloud.allowedTCPPorts = [
          22
          5173
          8080
        ];
        interfaces.wg_tcloud.allowedUDPPorts = [ 22 ];
        interfaces.wt0.allowedTCPPorts = [ 22 ];
        interfaces.wt0.allowedUDPPorts = [ 22 ];
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
          wg_tcloud = {
            privateKeyFile = config.sops.secrets."wireguard/game".path;
            table = "off";
            address = [ "fe80::102/64" ];
            postUp = ''
              ${pkgs.iproute2}/bin/ip addr add dev wg_tcloud 172.22.240.99/32 peer 172.22.240.96/27
              ${pkgs.iproute2}/bin/ip addr add dev wg_tcloud fd48:4b4:f3::3/128 peer fd48:4b4:f3::1/128
              ${pkgs.iproute2}/bin/ip link set multicast on dev wg_tcloud
            '';

            peers = [
              {
                endpoint = "43.156.66.157:22617";
                publicKey = shares.hosts-dict.tcloud.wg.public-key;
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

    # cloudflare-warp = {
    #   enable = true;
    # };
    v2raya.enable = true;

    sunshine = {
      enable = false;
      openFirewall = true;
      capSysAdmin = true;
    };
    # litellm = {
    #   enable = true;
    #   environmentFile = "";
    #   settings = {
    #     model_list = [
    #       {
    #         model_name = "github_copilot/gpt-4";
    #         litellm_params = {
    #           model = "github_copilot/gpt-4";
    #         };
    #       }
    #     ];
    #   };
    # };
    postgresql = {
      enable = true;
      package = pkgs.postgresql_18_jit;
      authentication = ''
        local all all trust
        host  all  all 0.0.0.0/0 scram-sha-256
      '';
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
        # shared_preload_libraries = "pg_cron";
        # "cron.database_name" = "postgres";
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

    # FRP client — disabled, token managed by sops when re-enabled
    frp = {
      instances."" = {
        enable = false;
        role = "client";
        settings = {
          serverAddr = "tcloud.${config.networking.domain}";
          serverPort = 7000;
          # auth.token will be injected from sops when frp is re-enabled
          # See: sops.secrets."frp/token"
        };
      };
    };

    netbird.enable = true;
    babeld = {
      interfaces = {
        wg_tcloud = {
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

  programs = {
    ydotool = {
      enable = true;
    };
    # clash-verge = {
    #   enable = true;
    # };
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
