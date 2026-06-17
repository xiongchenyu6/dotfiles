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
    #ezModules.falcon-sensor
    ezModules.wayland
    ezModules.openfortivpn-config
    ezModules.nas
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
    ./dnf-native.nix
    ./hashtopolis-agent.nix
    # ./vast-cli.nix  # Moved to home-manager module
  ];

  sops.secrets."wireguard/game" = { };

  system.nixos.tags = [
    "nvidia"
    "gui"
  ];

  nixpkgs.overlays = [
    (_: prev: {
      nvidia-vaapi-driver = prev.nvidia-vaapi-driver.overrideAttrs (_: {
        version = "0.0.17-chrome-stream-format-switch";
        src = prev.fetchFromGitHub {
          owner = "imperishableSecret";
          repo = "nvidia-vaapi-driver";
          rev = "288a7ba79d47219ea6dea737ec8d684b53a8de36";
          hash = "sha256-wxgdf+Gln1Tv7S/EbVUNOpxJ4Z0Ew4VudBglX7d5XD8=";
        };
      });
    })
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
      inputs.lazynixos.packages.${pkgs.stdenv.hostPlatform.system}.default
    ];
  };

  hardware.nvidia.package = config.boot.kernelPackages.nvidiaPackages.latest;
  boot = {
    binfmt.emulatedSystems = [ "aarch64-linux" ];
    initrd.kernelModules = [
      "vfio_pci"
      "vfio"
      "vfio_iommu_type1"
    ];
    kernelModules = [
      "sp5100_tco" # AMD watchdog module for Legion 16ach6h
      "acpi_call" # exploratory: try to clear EC's persistent camera-disabled flag left by Lenovo Vantage
    ];
    kernelParams = [
      "iommu=pt"
      "xhci_hcd.quirks=270336"
      "usbcore.autosuspend=-1"
      #"usbcore.old_scheme_first=1"
      "sp5100_tco.nowayout=1" # Prefer automatic reboot over hard-freeze requiring manual power cycle
    ];

    extraModulePackages = with config.boot.kernelPackages; [ acpi_call ];

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

  # Hardware watchdog configuration: prefer automatic reboot over staying frozen.
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
        safe-exit = true; # Disable watchdog on clean exit when nowayout is not active
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
      # game is a local workstation, not part of the autolife.ai inner zone.
      # Unsetting the domain keeps gethostname() == $HOST (short name) so
      # ghostty's OSC 7 isLocal() check accepts cwd reports from the shell.
      domain = null;
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
        interfaces.wg_ora.allowedTCPPorts = [
          22
          5173
          8080
          8765
        ];
        interfaces.wg_ora.allowedUDPPorts = [ 22 ];
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
          wg_ora = {
            privateKeyFile = config.sops.secrets."wireguard/game".path;
            table = "off";
            address = [ "fe80::102/64" ];
            postUp = ''
              ${pkgs.iproute2}/bin/ip addr add dev wg_ora 172.22.240.99/32 peer 172.22.240.96/27
              ${pkgs.iproute2}/bin/ip addr add dev wg_ora fd48:4b4:f3::3/128 peer fd48:4b4:f3::1/128
              ${pkgs.iproute2}/bin/ip link set multicast on dev wg_ora
            '';

            peers = [
              {
                endpoint = "213.35.117.232:22616";
                publicKey = shares.hosts-dict.oracle-amd-002.wg.public-key;
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
      enable = true;
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
          serverAddr = "autolife.ai";
          serverPort = 7000;
          # auth.token will be injected from sops when frp is re-enabled
          # See: sops.secrets."frp/token"
        };
      };
    };

    netbird.enable = true;
    babeld = {
      interfaces = {
        wg_ora = {
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

  # Sunshine's NVENC encoder dlopen()s libcuda.so.1, which lives in
  # /run/opengl-driver/lib on NixOS. Without this on the user-service env,
  # CUDA fails to load and Sunshine silently falls back to libx264 (CPU).
  systemd.user.services.sunshine.environment.LD_LIBRARY_PATH = "/run/opengl-driver/lib";

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
      "freeman.xiong" =
        let
          wechatHiDpi = pkgs.writeShellScriptBin "wechat" ''
            export QT_ENABLE_HIGHDPI_SCALING=1
            export QT_SCALE_FACTOR=''${WECHAT_SCALE_FACTOR:-1.5}
            exec ${pkgs.wechat}/bin/wechat "$@"
          '';
        in
        {
          home.packages = [
            wechatHiDpi
          ];

          xdg.dataFile."applications/wechat.desktop" = {
            text = ''
              [Desktop Entry]
              Type=Application
              Name=WeChat
              Comment=WeChat desktop client
              Exec=wechat %U
              Icon=${pkgs.wechat}/share/icons/hicolor/256x256/apps/wechat.png
              Categories=Network;InstantMessaging;
              StartupNotify=true
              Terminal=false
            '';
          };

          systemd.user.services.codex-remote-control = {
            Unit = {
              Description = "Codex remote control bridge";
              After = [ "network-online.target" ];
            };

            Service = {
              Type = "simple";
              WorkingDirectory = "%h";
              ExecStartPre = "${pkgs.writeShellScript "codex-remote-control-pre-start" ''
                mkdir -p "$HOME/.codex"
                ${pkgs.codex}/bin/codex features enable remote_control
              ''}";
              ExecStart = "${pkgs.codex}/bin/codex remote-control";
              Restart = "always";
              RestartSec = 5;
              StandardOutput = "append:%h/.codex/remote-control.log";
              StandardError = "append:%h/.codex/remote-control.log";
            };

            Install = {
              WantedBy = [ "default.target" ];
            };
          };

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
