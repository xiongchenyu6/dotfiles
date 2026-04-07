{
  inputs,
  modulesPath,
  lib,
  pkgs,
  config,
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
    ezModules.sing-box
    srvos.nixosModules.server
    ezModules.mixins-nginx
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
    xiongchenyu6.nixosModules.cc-gateway
    ./hardware-configuration.nix
  ];

  services = {
    openssh = {
      enable = true;
      authorizedKeysCommand = "/run/wrappers/bin/kanidm_ssh_authorizedkeys %u";
      authorizedKeysCommandUser = "nobody";
      settings = {
        UsePAM = true;
      };
    };
  };

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

  environment.systemPackages = with pkgs; [
    inputs.xiongchenyu6.packages.x86_64-linux.cc-gateway
  ];

  services.cc-gateway = {
    enable = true;
    port = 18443;
    openFirewall = false;
    configFile = config.sops.templates."cc-gateway-config".path;
  };
}
