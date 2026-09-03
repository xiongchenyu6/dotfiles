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
let
  codexPackage = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.codex;
in
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
    nixos-hardware.nixosModules.lenovo-legion-16ach6h-hybrid
    srvos.nixosModules.desktop
    srvos.nixosModules.mixins-tracing
    # Import Hashtopolis agent module from NUR packages
    xiongchenyu6.nixosModules.hashtopolis-agent
    xiongchenyu6.nixosModules.codexpro
    xiongchenyu6.nixosModules.happier
    {
      topology.self.interfaces.home = {
        type = "wireguard";
        addresses = [ "172.22.240.99/27" ];
      };
    }
    ./dnf-native.nix
    ./hashtopolis-agent.nix
    ./codexpro.nix
    ./happier.nix
    ./waydroid.nix
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

  hardware.nvidia = {
    package = config.boot.kernelPackages.nvidiaPackages.latest;
    dynamicBoost.enable = true;
  };
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

  # sing-box is an opt-in travel VPN. Keep both the service and its TUN absent
  # at boot; pon/poff are the only normal start/stop path.
  systemd.services.sing-box = {
    wantedBy = lib.mkForce [ ];
    unitConfig.X-OnlyManualStart = true;
  };

  # Let this user toggle only the sing-box unit without a password prompt.
  security.polkit.extraConfig = ''
    polkit.addRule(function(action, subject) {
      if (action.id == "org.freedesktop.systemd1.manage-units" &&
          subject.user == "freeman.xiong" &&
          action.lookup("unit") == "sing-box.service" &&
          (action.lookup("verb") == "start" ||
           action.lookup("verb") == "restart" ||
           action.lookup("verb") == "stop")) {
        return polkit.Result.YES;
      }
    });
  '';

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
      # the terminal's OSC 7 isLocal() check accepts cwd reports from the shell.
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
          4242 # lan-mouse (software KVM with gz-pc)
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
    # 声明式代理客户端(替代 v2rayA):服务默认不启动。pon 启动分流 TUN,
    # poff 停止服务并移除 TUN。
    sing-box = {
      enable = true;
      settings =
        let
          # Non-secret node metadata imported from
          # ~/Downloads/karing-hy2/links.txt. All five links use the same
          # password, which remains encrypted in SOPS instead of entering the
          # Nix store.
          hy2Nodes = [
            {
              tag = "hy2-lubancat";
              server = "203.116.95.146";
              serverName = "hy2-lubancat.panda.qzz.io";
            }
            {
              tag = "hy2-oracle-amd-001";
              server = "213.35.97.233";
              serverName = "panda.qzz.io";
            }
            {
              tag = "hy2-oracle-amd-002";
              server = "213.35.117.232";
              serverName = "panda.qzz.io";
            }
            {
              tag = "hy2-sg-office";
              server = "101.78.126.6";
              serverName = "hy2-sg.panda.qzz.io";
            }
            {
              # Lowest-priority emergency fallback: this node has the lowest
              # latency but measured only ~150 KB/s and had the 16:00 outage.
              tag = "hy2-jtti-sg";
              server = "45.194.18.75";
              serverName = "hy2-jtti-sg.panda.qzz.io";
            }
          ];
        in
        {
          log.level = "warn";
          dns = {
            servers = [
              {
                # DHCP discovery times out on some travel routers. Use AliDNS
                # directly for mainland domains instead of inheriting fake-IP DNS.
                type = "https";
                tag = "dns-direct";
                server = "223.5.5.5";
                detour = "direct";
                tls.server_name = "dns.alidns.com";
              }
              {
                type = "https";
                tag = "dns-proxy";
                server = "1.1.1.1";
                detour = "proxy";
                tls.server_name = "cloudflare-dns.com";
              }
            ];
            rules = [
              {
                # Never let a mainland resolver synthesize fake IPs for Claude.
                domain_suffix = [
                  "anthropic.com"
                  "claude.ai"
                  "claude.com"
                  "claudeusercontent.com"
                ];
                action = "route";
                server = "dns-proxy";
              }
              {
                # Keep mainland sites fast while foreign DNS uses DoH over Hysteria2.
                rule_set = [ "geosite-cn" ];
                action = "route";
                server = "dns-direct";
              }
            ];
            final = "dns-proxy";
            # This host has no public IPv6 route. Returning AAAA made Bun/Claude
            # report IPv6 reachability failures as certificate verification errors.
            strategy = "ipv4_only";
            reverse_mapping = true;
          };
          inbounds = [
            {
              type = "tun";
              tag = "tun-in";
              interface_name = "sing-tun";
              # Avoid the LAN, Docker, libvirt, NetBird, and WireGuard ranges.
              address = [ "10.255.0.1/30" ];
              stack = "system";
              auto_route = true;
              auto_redirect = true;
              strict_route = true;
            }
          ];
          # Clash API + metacubexd 面板:http://127.0.0.1:9090/ui
          experimental.clash_api = {
            external_controller = "127.0.0.1:9090";
            external_ui = "${pkgs.metacubexd}";
          };
          outbounds = [
            {
              # Native URLTest has no bandwidth metric. Keep the measured
              # high-quality nodes first and use a wide latency tolerance as
              # ordered failover; an unavailable node is removed immediately.
              type = "urltest";
              tag = "proxy";
              outbounds = map (node: node.tag) hy2Nodes;
              url = "https://api.anthropic.com/";
              interval = "15s";
              tolerance = 2000;
              idle_timeout = "10m";
              interrupt_exist_connections = true;
            }
          ]
          ++ map (node: {
            type = "hysteria2";
            inherit (node) tag server;
            server_port = 8443;
            password._secret = config.sops.secrets."sing-box/HYSTERIA2_PASSWORD".path;
            tls = {
              enabled = true;
              # Every node uses a public Let's Encrypt certificate.
              server_name = node.serverName;
            };
          }) hy2Nodes
          ++ [
            {
              type = "direct";
              tag = "direct";
              domain_resolver = "dns-direct";
            }
          ];
          route = {
            # TUN 出站必须绑定系统检测到的默认物理接口,否则会套娃回 TUN。
            auto_detect_interface = true;
            # Private and mainland traffic is bypassed below; everything else
            # uses Hysteria2 while the opt-in service is running.
            final = "proxy";
            rules = [
              {
                action = "sniff";
              }
              {
                protocol = "dns";
                action = "hijack-dns";
              }
              {
                # Claude 官方网络清单中的核心服务。显式放在 CN 分流前,
                # 避免污染 DNS、GeoIP 误判或 CDN 漂移导致偶发直连。
                domain_suffix = [
                  "anthropic.com"
                  "claude.ai"
                  "claude.com"
                  "claudeusercontent.com"
                ];
                action = "route";
                outbound = "proxy";
              }
              {
                # NetBird uses RFC 6598 CGNAT addresses, which ip_is_private
                # deliberately does not include.
                ip_cidr = [ "100.64.0.0/10" ];
                action = "bypass";
                outbound = "direct";
              }
              {
                ip_is_private = true;
                action = "bypass";
                outbound = "direct";
              }
              {
                rule_set = [
                  "geoip-cn"
                  "geosite-cn"
                ];
                action = "bypass";
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
          elementDesktop = pkgs.element-desktop.override {
            commandLineArgs = "--password-store=gnome-libsecret";
          };
          wechatHiDpi = pkgs.writeShellScriptBin "wechat" ''
            export QT_ENABLE_HIGHDPI_SCALING=1
            export QT_SCALE_FACTOR=''${WECHAT_SCALE_FACTOR:-1.5}
            exec ${pkgs.wechat}/bin/wechat "$@"
          '';
        in
        {
          home.packages = [
            elementDesktop
            wechatHiDpi
          ];

          xdg.mimeApps = {
            enable = true;
            defaultApplications = {
              "x-scheme-handler/element" = [ "element-desktop.desktop" ];
              "x-scheme-handler/io.element.desktop" = [ "element-desktop.desktop" ];
            };
          };

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
                ${codexPackage}/bin/codex features enable remote_control
              ''}";
              ExecStart = "${codexPackage}/bin/codex remote-control";
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
