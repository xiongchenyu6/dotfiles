{
  config,
  inputs,
  modulesPath,
  lib,
  pkgs,
  ezModules,
  ...
}:
{
  imports = with inputs; [
    hermes-agent.nixosModules.default
    disko.nixosModules.disko
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")
    ezModules.root
    ezModules."freeman.xiong"
    ezModules.core
    ezModules.server
    ezModules.acme
    ezModules.sing-box
    srvos.nixosModules.server
    ezModules.mixins-nginx
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
    xiongchenyu6.nixosModules.cc-gateway
    ./hardware-configuration.nix
  ];

  sops.secrets."cloudflared/tunnel-credentials" = { };

  # hermes-agent moved here from amd-002 (frees amd-002 RAM + isolates the
  # EOL-nodejs build to this now-idle box). The XIAOMI_API_KEY / telegram-token
  # SOPS keys already live in secrets/common.yaml, encrypted to all hosts
  # incl. oracle-amd-001 — no re-encryption needed.
  sops.templates."hermes-env".content = ''
    XIAOMI_API_KEY=${config.sops.placeholder."api-keys/XIAOMI_API_KEY"}
    TELEGRAM_BOT_TOKEN=${config.sops.placeholder."zeroclaw/telegram_bot_token"}
  '';
  sops.secrets."api-keys/XIAOMI_API_KEY".owner = "root";
  sops.secrets."zeroclaw/telegram_bot_token".owner = "root";

  # cc-gateway moved here from amd-002 (frees amd-002 RAM for the new IB Gateway
  # container). Port 18443 is free on this box. The cc-gateway/* SOPS keys already
  # live in secrets/common.yaml, encrypted to all hosts incl. oracle-amd-001 — no
  # re-encryption needed.
  sops.templates."cc-gateway-config" = {
    content = ''
      {
        "server": {
          "port": 18443
        },
        "upstream": {
          "url": "https://api.anthropic.com"
        },
        "identity": {
          "device_id": "${config.sops.placeholder."cc-gateway/device_id"}",
          "email": "${config.sops.placeholder."cc-gateway/email"}"
        },
        "oauth": {
          "refresh_token": "${config.sops.placeholder."cc-gateway/refresh_token"}"
        },
        "auth": {
          "tokens": ${config.sops.placeholder."cc-gateway/client_tokens"}
        },
        "env": {
          "platform": "linux",
          "platform_raw": "linux",
          "arch": "x86_64",
          "node_version": "v22.0.0",
          "terminal": "xterm-256color",
          "package_managers": "npm",
          "runtimes": "node",
          "is_running_with_bun": false,
          "is_ci": false,
          "is_claude_ai_auth": true,
          "version": "2.1.81",
          "version_base": "2.1.81",
          "build_time": "2026-03-20T21:26:18Z",
          "deployment_environment": "nixos",
          "vcs": "git"
        },
        "prompt_env": {
          "platform": "linux",
          "shell": "bash",
          "os_version": "NixOS",
          "working_dir": "/var/lib/cc-gateway"
        },
        "process": {
          "constrained_memory": 34359738368,
          "rss_range": [300000000, 500000000],
          "heap_total_range": [40000000, 80000000],
          "heap_used_range": [100000000, 200000000]
        },
        "logging": {
          "level": "info",
          "audit": true
        }
      }
    '';
    owner = "cc-gateway";
    group = "cc-gateway";
    mode = "0400";
  };

  sops.secrets."cc-gateway/email".owner = "cc-gateway";
  sops.secrets."cc-gateway/device_id".owner = "cc-gateway";
  sops.secrets."cc-gateway/refresh_token".owner = "cc-gateway";
  sops.secrets."cc-gateway/client_tokens".owner = "cc-gateway";

  users.users.cc-gateway = {
    isSystemUser = true;
    group = "cc-gateway";
    home = "/var/lib/cc-gateway";
    createHome = true;
  };
  users.groups.cc-gateway = { };

  environment = {
    systemPackages = [
      inputs.xiongchenyu6.packages.x86_64-linux.cc-gateway
      pkgs.cloudflared
      pkgs.nix
    ];
  };

  networking = {
    firewall = {
      allowedTCPPorts = [
        80
        443
        636
      ];
      allowedUDPPorts = [ 53 ];
    };
  };

  security = {
    acme = {
      certs = {
        ${config.networking.domain} = {
          domain = "${config.networking.domain}";
          extraDomainNames = [ "*.${config.networking.domain}" ];
          group = "acme";
        };
        ai = {
          group = "nginx";
          reloadServices = [ "nginx.service" ];
        };
      };
    };
  };

  services.hermes-agent = {
    enable = true;
    settings = {
      model = {
        default = "mimo-v2.5-pro";
        provider = "xiaomi";
      };
      # User-authored skills migrated from the old zeroclaw workspace. Hermes
      # reads these alongside its built-in skill library; skill creation still
      # writes to $HERMES_HOME/skills/. State dir /var/lib/hermes is rsynced
      # from amd-002 at migration time (see deploy notes).
      skills.external_dirs = [ "/var/lib/hermes/custom-skills" ];
    };
    # Non-secret env vars (bot allowlist + provider endpoint). Secrets via environmentFiles.
    environment = {
      TELEGRAM_ALLOWED_USERS = "5368588092,5369058954";
      XIAOMI_BASE_URL = "https://token-plan-cn.xiaomimimo.com/v1";
    };
    environmentFiles = [ config.sops.templates."hermes-env".path ];
  };

  services.cc-gateway = {
    enable = true;
    port = 18443;
    openFirewall = false;
    configFile = config.sops.templates."cc-gateway-config".path;
  };
}
