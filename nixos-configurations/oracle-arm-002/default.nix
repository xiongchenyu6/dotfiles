{
  inputs,
  modulesPath,
  lib,
  pkgs,
  ezModules,
  ...
}:
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
    srvos.nixosModules.mixins-nginx
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
    # Import Hashtopolis server module from NUR packages
    xiongchenyu6.nixosModules.hashtopolis-server
    ./disk-config.nix
    ./hardware-configuration.nix
    ./hashtopolis.nix # Hashtopolis server configuration

  ];

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
    ++ (with pkgs; [
      nodejs_22
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

  networking.firewall.allowedTCPPorts = [ 6080 ];

  # OpenClaw — personal AI assistant
  users.users.openclaw = {
    isSystemUser = true;
    group = "openclaw";
    home = "/var/lib/openclaw";
    createHome = true;
  };
  users.groups.openclaw = { };

  systemd.services.openclaw = {
    description = "OpenClaw Gateway Service";
    after = [
      "network-online.target"
      "sops-nix.service"
    ];
    wants = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];

    path = with pkgs; [
      nodejs_22
      git
      bash
      coreutils
      gnused
      gnutar
      gzip
      findutils
      gnugrep
      nix
      systemd
      sudo
    ];

    # Install openclaw globally on first start or update
    preStart = ''
      export HOME=/var/lib/openclaw
      export NPM_CONFIG_PREFIX=/var/lib/openclaw/.npm-global
      export PATH="$NPM_CONFIG_PREFIX/bin:$PATH"
      if [ ! -x "$NPM_CONFIG_PREFIX/bin/openclaw" ]; then
        echo "Installing openclaw..."
        ${pkgs.nodejs_22}/bin/npm install -g openclaw@latest --prefix "$NPM_CONFIG_PREFIX" --ignore-scripts
      fi

      # Inject API key into Environment file
      echo "NVIDIA_API_KEY=$(cat /run/secrets/zeroclaw/nvidia_api_key)" > /var/lib/openclaw/.openclaw-env
      echo "GEMINI_API_KEY=$(cat /run/secrets/api-keys/GEMINI_API_KEY)" >> /var/lib/openclaw/.openclaw-env

      # Inject telegram bot token and models into openclaw config if it doesn't exist
      mkdir -p /var/lib/openclaw/.openclaw
      if [ ! -f /var/lib/openclaw/.openclaw/openclaw.json ]; then
        cat > /var/lib/openclaw/.openclaw/openclaw.json << CONF
      {
        "gateway": {
          "port": 18789,
          "bind": "lan",
          "mode": "local"
        },
         "agents": {
           "defaults": {
             "model": {
               "primary": "volcengine/ark-code-latest",
               "fallbacks": [
                 "google/gemini-2.5-flash"
               ]
              }
            }
         },
        "models": {
          "providers": {
            "volcengine": {
              "baseUrl": "https://ark.cn-beijing.volces.com/api/coding/v3",
              "api": "openai-completions",
              "auth": "api-key",
              "apiKey": "$(cat /run/secrets/api-keys/VOLCENGINE_API_KEY)",
              "models": [
                {
                  "id": "ark-code-latest",
                  "name": "Volcengine Ark Code Latest",
                  "input": ["text"],
                  "contextWindow": 65536,
                  "maxTokens": 8192
                }
              ]
            },
            "siliconflow": {
              "baseUrl": "https://api.siliconflow.cn/v1",
              "api": "openai-completions",
              "auth": "api-key",
              "apiKey": "$(cat /run/secrets/api-keys/SILICON_FLOW)",
              "models": [
                {
                  "id": "deepseek-ai/DeepSeek-V3",
                  "name": "DeepSeek V3",
                  "input": ["text"],
                  "contextWindow": 65536,
                  "maxTokens": 8192
                },
                {
                  "id": "Pro/MiniMaxAI/MiniMax-M2.5",
                  "name": "MiniMax M2.5 (Pro)",
                  "input": ["text"],
                  "contextWindow": 131072,
                  "maxTokens": 4096
                }
              ]
            },
            "nvidia": {
              "baseUrl": "https://integrate.api.nvidia.com/v1",
              "api": "openai-completions",
              "auth": "api-key",
              "apiKey": "$(cat /run/secrets/zeroclaw/nvidia_api_key)",
              "models": [
                {
                  "id": "minimaxai/minimax-m2.5",
                  "name": "MiniMax M2.5",
                  "input": ["text"],
                  "contextWindow": 131072,
                  "maxTokens": 4096
                }
              ]
            }
          }
        },
        "tools": {
          "profile": "full",
          "allow": ["*"],
           "elevated": {
             "enabled": true,
             "allowFrom": {
               "telegram": ["5368588092", "5369058954", "5293993503"]
             }
           }
        },
         "channels": {
           "telegram": {
             "botToken": "$(cat /run/secrets/zeroclaw/telegram_bot_token)",
             "dmPolicy": "allowlist",
             "groupPolicy": "allowlist",
             "allowFrom": ["5368588092", "5369058954"],
             "groupAllowFrom": ["5368588092", "5369058954", "5293993503"]
           }
         },
        "mcp": {
          "servers": {
            "xiaohongshu-mcp": {
              "url": "http://127.0.0.1:18060/mcp"
            }
          }
        }
      }
      CONF
      fi
    '';

    script = ''
      export HOME=/var/lib/openclaw
      export NPM_CONFIG_PREFIX=/var/lib/openclaw/.npm-global
      export PATH="$NPM_CONFIG_PREFIX/bin:$PATH"
      export GEMINI_API_KEY=$(cat /run/secrets/api-keys/GEMINI_API_KEY)
      export NVIDIA_API_KEY=$(cat /run/secrets/zeroclaw/nvidia_api_key)
      exec openclaw gateway --port 18789
    '';

    serviceConfig = {
      User = "root";
      Group = "root";
      WorkingDirectory = "/var/lib/openclaw";
      StateDirectory = "openclaw";
      Restart = "always";
      RestartSec = 5;
    };
  };

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

  systemd.services.x11vnc = {
    description = "X11 VNC server";
    after = [
      "network-online.target"
      "xvfb.service"
    ];
    wants = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];
    requires = [ "xvfb.service" ];

    script = ''
      exec ${pkgs.x11vnc}/bin/x11vnc -display :99 -forever -shared -nopw -listen localhost -rfbport 5900
    '';

    serviceConfig = {
      User = "openclaw";
      Group = "openclaw";
      Restart = "always";
      RestartSec = 5;
      Type = "simple";
    };
  };

  systemd.services.novnc = {
    description = "NoVNC web VNC client";
    after = [
      "network-online.target"
      "x11vnc.service"
    ];
    wants = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];
    requires = [ "x11vnc.service" ];

    script = ''
      exec ${pkgs.novnc}/bin/novnc --vnc localhost:5900 --listen 0.0.0.0:6080
    '';

    serviceConfig = {
      User = "openclaw";
      Group = "openclaw";
      Restart = "always";
      RestartSec = 5;
      Type = "simple";
    };
  };

  systemd.services.xiaohongshu-mcp = {
    description = "Xiaohongshu MCP service";
    after = [
      "network-online.target"
      "xvfb.service"
    ];
    wants = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];
    requires = [ "xvfb.service" ];

    environment = {
      DISPLAY = ":99";
    };

    script = ''
      exec ${
        inputs.xiongchenyu6.packages."aarch64-linux".xiaohongshu-mcp
      }/bin/xiaohongshu-mcp -headless=false
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
