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
    xiongchenyu6.nixosModules.codexpro
    {
      topology.self.interfaces.home = {
        type = "wireguard";
        addresses = [ "172.22.240.99/27" ];
      };
    }
    ./dnf-native.nix
    ./hashtopolis-agent.nix
    ./codexpro.nix
    # ./vast-cli.nix  # Moved to home-manager module
  ];

  sops.secrets."wireguard/game" = { };
  sops.secrets."sing-box/HYSTERIA2_PASSWORD" = { };

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
      inputs.xiongchenyu6.packages.${pkgs.stdenv.hostPlatform.system}.larksuite-cli
      mangohud # in-game perf overlay (FPS/frametime/temps); ships 32-bit mangohud too
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
        # Reboot instead of staying frozen: turn soft hangs / oops into a
        # panic, and make panics auto-reboot (systemd RuntimeWatchdog only
        # catches a full kernel lockup, not a GPU/compositor freeze).
        "kernel.panic" = 10; # auto-reboot 10s after a panic
        "kernel.panic_on_oops" = 1; # an oops becomes a panic
        "kernel.hung_task_panic" = 1; # task stuck in D-state too long -> panic
        "kernel.hung_task_timeout_secs" = 120;
        "kernel.softlockup_panic" = 1; # CPU soft lockup -> panic
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
          8443
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
                publicKey = shares.hosts.oracle-amd-002.wg.public-key;
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
    # 声明式代理客户端(替代 v2rayA):服务端(jtti-sg)只跑 hysteria2,
    # xray 不支持 hysteria2,直接用 sing-box。端口沿用 v2rayA 的
    # 20170/20171(mixed 同时接受 socks5 和 http),消费方不用改。
    sing-box = {
      enable = true;
      settings = {
        log.level = "warn";
        inbounds = [
          {
            type = "mixed";
            tag = "in-20170";
            listen = "127.0.0.1";
            listen_port = 20170;
          }
          {
            type = "mixed";
            tag = "in-20171";
            listen = "127.0.0.1";
            listen_port = 20171;
          }
        ];
        # Clash API + metacubexd 面板:http://127.0.0.1:9090/ui
        # cache_file 记住面板里手选的节点,重启不丢
        experimental = {
          clash_api = {
            external_controller = "127.0.0.1:9090";
            external_ui = "${pkgs.metacubexd}";
          };
          cache_file.enabled = true;
        };
        outbounds = [
          {
            # 面板上的总开关:默认走 auto 测速,可手动切到具体节点或 direct
            type = "selector";
            tag = "proxy";
            outbounds = [
              "auto"
              "hy2-jtti"
              "direct"
            ];
            default = "auto";
          }
          {
            # 以后加节点:outbounds 塞进这个列表即可自动测速选优
            type = "urltest";
            tag = "auto";
            outbounds = [ "hy2-jtti" ];
          }
          {
            type = "hysteria2";
            tag = "hy2-jtti";
            server = "45.194.18.75";
            server_port = 8443;
            password._secret = config.sops.secrets."sing-box/HYSTERIA2_PASSWORD".path;
            tls = {
              enabled = true;
              # 服务端是 ansible 生成的自签证书(CN hy2-jtti-sg.panda.qzz.io),
              # 无法走公共 CA 校验;hysteria2 本身有密码认证
              server_name = "hy2-jtti-sg.panda.qzz.io";
              insecure = true;
            };
          }
          {
            type = "direct";
            tag = "direct";
          }
        ];
        route = {
          final = "proxy";
          rules = [
            {
              ip_is_private = true;
              outbound = "direct";
            }
            {
              rule_set = [
                "geoip-cn"
                "geosite-cn"
              ];
              outbound = "direct";
            }
          ];
          rule_set = [
            {
              tag = "geoip-cn";
              type = "local";
              format = "binary";
              path = "${pkgs.sing-geoip}/share/sing-box/rule-set/geoip-cn.srs";
            }
            {
              tag = "geosite-cn";
              type = "local";
              format = "binary";
              path = "${pkgs.sing-geosite}/share/sing-box/rule-set/geosite-cn.srs";
            }
          ];
        };
      };
    };

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
    # Gaming: Wayland micro-compositor for fullscreen/scaling/tearing control
    # under niri, plus GameMode for on-demand CPU/GPU performance tuning.
    gamescope = {
      enable = true;
      capSysNice = true;
    };
    gamemode = {
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
