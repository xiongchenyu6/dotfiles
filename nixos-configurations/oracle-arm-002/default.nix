{
  inputs,
  modulesPath,
  lib,
  config,
  pkgs,
  ezModules,
  ...
}:
let
  mcporter = inputs.llm-agents.packages.${pkgs.system}.mcporter;

  # Shared packages available to both system environment and openclaw service
  sharedToolPkgs = with pkgs; [
    nodejs_25
    bash
    coreutils
    gnused
    gnutar
    gzip
    findutils
    gnugrep
    git
    scrot
    imagemagick
    mcporter
  ];
in
{
  imports = with inputs; [
    disko.nixosModules.disko
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")
    ezModules.root
    ezModules."freeman.xiong"
    ezModules.core
    ezModules.server
    ezModules.acme
    ezModules.datadog-agent
    srvos.nixosModules.server
    ezModules.mixins-nginx
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
    # Import Hashtopolis server module from NUR packages
    inputs.openclaw.nixosModules.openclaw-gateway
    xiongchenyu6.nixosModules.hashtopolis-server
    xiongchenyu6.nixosModules.xiaohongshu-mcp
    ./disk-config.nix
    ./hardware-configuration.nix
    ./hashtopolis.nix # Hashtopolis server configuration

  ];

  sops.templates."openclaw-env".content = ''
    VOLCENGINE_API_KEY=${config.sops.placeholder."api-keys/VOLCENGINE_API_KEY"}
    SILICON_FLOW_API_KEY=${config.sops.placeholder."api-keys/SILICON_FLOW"}
    NVIDIA_API_KEY=${config.sops.placeholder."zeroclaw/nvidia_api_key"}
    TELEGRAM_BOT_TOKEN=${config.sops.placeholder."zeroclaw/telegram_bot_token"}
    GEMINI_API_KEY=${config.sops.placeholder."api-keys/GEMINI_API_KEY"}
  '';

  sops.secrets."api-keys/GEMINI_API_KEY".owner = "root";
  sops.secrets."api-keys/SILICON_FLOW".owner = "root";
  sops.secrets."api-keys/VOLCENGINE_API_KEY".owner = "root";
  sops.secrets."zeroclaw/nvidia_api_key".owner = "openclaw";
  sops.secrets."zeroclaw/telegram_bot_token".owner = "openclaw";

  boot = {
    loader.grub = {
      # no need to set devices, disko will add all devices that have a EF02 partition to the list already
      # devices = [ ];
      efiSupport = true;
      efiInstallAsRemovable = true;
    };
    kernel = {
      sysctl = {
        "net.ipv4.ip_forward" = 1;
      };
    };
  };

  users = {
    users = {
      root = {
        openssh = {
          authorizedKeys.keys = [
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINCXyKpDU97Js6YcRIL+8M/vOdPwRKlMpTzmjGLJmCHT rachelove24@Rachels-MacBook-Air-2.local"
          ];
        };
      };
    };
  };

  environment.systemPackages =
    map lib.lowPrio [
      pkgs.curl
      pkgs.gitMinimal
    ]
    ++ sharedToolPkgs
    ++ (with pkgs; [
      xvfb-run
      x11vnc
      novnc
      chromium
      xorg-server
      inputs.xiongchenyu6.packages."aarch64-linux".xiaohongshu-mcp
    ]);

  nixpkgs = {
    hostPlatform = "aarch64-linux";
  };

  networking.firewall.allowedTCPPorts = [
    6080
  ];

  services.xiaohongshu-mcp = {
    enable = true;
    port = 18060;
    display = ":99";
    workingDirectory = "/var/lib/openclaw";
    cookiesPath = "/var/lib/openclaw/.openclaw/workspace/cookies.json";
    user = "root";
    group = "root";
  };

  services.openclaw-gateway = {
    enable = true;
    user = "root";
    group = "root";
    createUser = false;
    port = 18789;
    servicePath =
      sharedToolPkgs
      ++ (with pkgs; [
        nix
        systemd
        sudo
        file
        wget
      ]);
    environmentFiles = [ config.sops.templates."openclaw-env".path ];

    config = {
      gateway = {
        port = 18789;
        bind = "lan";
        mode = "local";
      };
      approvals = {
        exec = {
          enabled = false;
        };
      };
      agents = {
        defaults = {
          model = {
            primary = "volcengine/ark-code-latest";
            fallbacks = [
              "google/gemini-2.5-flash"
            ];
          };
        };
      };
      models = {
        providers = {
          volcengine = {
            baseUrl = "https://ark.cn-beijing.volces.com/api/coding/v3";
            api = "openai-completions";
            auth = "api-key";
            apiKey = "\${VOLCENGINE_API_KEY}";
            models = [
              {
                id = "ark-code-latest";
                name = "Volcengine Ark Code Latest";
                input = [ "text" ];
                contextWindow = 65536;
                maxTokens = 8192;
              }
            ];
          };
          siliconflow = {
            baseUrl = "https://api.siliconflow.cn/v1";
            api = "openai-completions";
            auth = "api-key";
            apiKey = "\${SILICON_FLOW_API_KEY}";
            models = [
              {
                id = "deepseek-ai/DeepSeek-V3";
                name = "DeepSeek V3";
                input = [ "text" ];
                contextWindow = 65536;
                maxTokens = 8192;
              }
              {
                id = "Pro/MiniMaxAI/MiniMax-M2.5";
                name = "MiniMax M2.5 (Pro)";
                input = [ "text" ];
                contextWindow = 131072;
                maxTokens = 4096;
              }
            ];
          };
          nvidia = {
            baseUrl = "https://integrate.api.nvidia.com/v1";
            api = "openai-completions";
            auth = "api-key";
            apiKey = "\${NVIDIA_API_KEY}";
            models = [
              {
                id = "minimaxai/minimax-m2.5";
                name = "MiniMax M2.5";
                input = [ "text" ];
                contextWindow = 131072;
                maxTokens = 4096;
              }
            ];
          };
        };
      };
      tools = {
        profile = "full";
        allow = [ "*" ];
        elevated = {
          enabled = true;
          allowFrom = {
            telegram = [
              "5368588092"
              "5369058954"
            ];
          };
        };
      };
      channels = {
        telegram = {
          botToken = "\${TELEGRAM_BOT_TOKEN}";
          dmPolicy = "allowlist";
          groupPolicy = "allowlist";
          execApprovals = {
            enabled = false;
            approvers = [
              "5368588092"
              "5369058954"
            ];
          };
          groups = {
            "-1003475261813" = {
              allowFrom = [
                "5368588092"
                "5369058954"
              ];
              requireMention = false;
            };
          };
          allowFrom = [
            "5368588092"
            "5369058954"
          ];
          groupAllowFrom = [
            "5368588092"
            "5369058954"
          ];
        };
      };
    };
  };

  systemd.services.openclaw-gateway.serviceConfig = {
    StandardOutput = lib.mkForce "journal";
    StandardError = lib.mkForce "journal";
  };

  systemd.tmpfiles.rules =
    let
      mcporterJson = pkgs.writeText "mcporter.json" (
        builtins.toJSON {
          mcpServers = {
            xiaohongshu-mcp = {
              baseUrl = "http://127.0.0.1:18060/mcp";
            };
          };
          imports = [ ];
        }
      );
    in
    [
      # Create necessary directories
      "d /var/lib/openclaw/.openclaw/workspace 0755 root root - -"
      "d /var/lib/openclaw/config 0755 root root - -"
      "d /home/freeman.xiong/config 0755 freeman.xiong users - -"

      # Symlink the generated JSON files into the configs
      "d /root/config 0755 root root - -"
      "L+ /root/config/mcporter.json - - - - ${mcporterJson}"
      "L+ /var/lib/openclaw/config/mcporter.json - - - - ${mcporterJson}"
      "L+ /home/freeman.xiong/config/mcporter.json - - - - ${mcporterJson}"
    ];

  # OpenClaw — personal AI assistant
  users.users.openclaw = {
    isSystemUser = true;
    group = "openclaw";
    home = "/var/lib/openclaw";
    createHome = true;
  };
  users.groups.openclaw = { };

  # Virtual display support for browser automation
  systemd.services.xvfb = {
    description = "X virtual framebuffer for headless display";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];

    script = ''
      exec ${pkgs.xorg-server}/bin/Xvfb :99 -screen 0 1280x1024x24 -ac +extension GLX +render -noreset
    '';

    serviceConfig = {
      User = "openclaw";
      Group = "openclaw";
      Restart = "always";
      RestartSec = 5;
      Type = "simple";
    };
  };
}
