{
  inputs,
  modulesPath,
  lib,
  pkgs,
  config,
  ezModules,
  shares,
  ...
}:
let
  vpn-dev = "wg0";
  port = 22616;
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
    ezModules.sing-box
    srvos.nixosModules.server
    ezModules.mixins-nginx
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
    xiongchenyu6.nixosModules.cc-gateway
    hermes-agent.nixosModules.default
    ./hardware-configuration.nix
    {
      topology.self.interfaces.home = {
        type = "wireguard";
        addresses = [ "172.22.240.97/27" ];
      };
    }
  ];

  boot.initrd.kernelModules = [ "nvme" ];

  boot = {
    kernelPackages = lib.mkForce pkgs.linuxPackages_latest;
    tmp.cleanOnBoot = true;
    kernel = {
      sysctl = {
        "net.ipv4.ip_forward" = 1;
      };
    };
  };

  networking = {
    nat = {
      enable = true;
      enableIPv6 = true;
      externalInterface = "ens5";
      internalInterfaces = [ vpn-dev ];
    };

    firewall = {
      enable = true;
      trustedInterfaces = [ vpn-dev ];
      allowedTCPPorts = [
        22
        53
        80
        443
        179
        389
        636
        993
      ];
      allowedUDPPorts = [
        port
        53
        80
        179
        389
        636
        5353
      ];
      allowedUDPPortRanges = [
        {
          from = 49152;
          to = 65535;
        }
      ];
    };

    wg-quick = {
      interfaces =
        let
          privateKeyFile = config.sops.secrets."wireguard/tcloud".path;
          table = "off";
          mkPeer = mesh4: mesh6: publicKey: {
            inherit publicKey;
            allowedIPs = [
              "${mesh4}/32"
              "${mesh6}/128"
            ];
          };
        in
        {
          ${vpn-dev} = {
            inherit privateKeyFile table;
            listenPort = port;
            address = [
              "172.22.240.97/27"
              "fd48:4b4:f3::1/64"
            ];
            postUp = ''
              ${pkgs.iproute2}/bin/ip link set multicast on dev ${vpn-dev}
            '';
            peers = [
              (mkPeer "172.22.240.98" "fd48:4b4:f3::2"
                shares.hosts-dict.office.wg.public-key)
              (mkPeer "172.22.240.99" "fd48:4b4:f3::3"
                shares.hosts-dict.game.wg.public-key)
              (mkPeer "172.22.240.100" "fd48:4b4:f3::4"
                "nBEkTpn4kRYXS9r7beXh3uMYJBAq/534byXv8NsB8gM=")
              (mkPeer "172.22.240.101" "fd48:4b4:f3::5"
                "CAW6+atqM9xmCAZUaev3OZWbYKwjDNCHezyiBpiHmSg=")
              (mkPeer "172.22.240.102" "fd48:4b4:f3::6"
                "9WkAJx+EG3VifVLiMgD8+6CoCsBwSyWAMwtajoy/OTk=")
            ];
          };
          wg_kioubit = {
            inherit privateKeyFile table;
            address = [ "fe80::100/64" ];
            peers = [
              {
                endpoint = "hk1.g-load.eu:22616";
                publicKey = "sLbzTRr2gfLFb24NPzDOpy8j09Y6zI+a7NkeVMdVSR8=";
                allowedIPs = [
                  "10.0.0.0/8"
                  "172.20.0.0/14"
                  "172.31.0.0/16"
                  "fd00::/8"
                  "fe80::/64"
                  "fd48:4b4:f3::/48"
                  "ff02::1:6/128"
                  "224.0.0.251/32"
                  "ff02::fb/128"
                ];
              }
            ];
          };
        };
    };
  };

  sops.secrets."wireguard/tcloud" = { };

  # Secret env file consumed by hermes-agent. Stored under the legacy
  # `zeroclaw/telegram_bot_token` SOPS key — the YAML namespace in
  # secrets/common.yaml is unchanged; only the nix references migrated.
  sops.templates."hermes-env".content = ''
    XIAOMI_API_KEY=${config.sops.placeholder."api-keys/XIAOMI_API_KEY"}
    TELEGRAM_BOT_TOKEN=${config.sops.placeholder."zeroclaw/telegram_bot_token"}
  '';

  sops.templates."s3fs-passwd" = {
    content = "${config.sops.placeholder."s3fs/access_key"}:${
      config.sops.placeholder."s3fs/secret_key"
    }";
    mode = "0600";
    owner = "root";
    group = "root";
  };

  sops.secrets."s3fs/access_key" = { };
  sops.secrets."s3fs/secret_key" = { };

  sops.secrets."api-keys/XIAOMI_API_KEY".owner = "root";
  sops.secrets."zeroclaw/telegram_bot_token".owner = "root";

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
    s3fs
  ];

  services.cc-gateway = {
    enable = true;
    port = 18443;
    openFirewall = false;
    configFile = config.sops.templates."cc-gateway-config".path;
  };

  services.hermes-agent = {
    enable = true;
    settings = {
      model = {
        default = "mimo-v2.5-pro";
        provider = "xiaomi";
      };
      # User-authored skills migrated from the old zeroclaw workspace.
      # External dirs are read-only to hermes; skill creation still writes
      # to $HERMES_HOME/skills/. Hermes reads finnhub/flyclaw/self-improving/
      # xiaohongshu-mcp from here alongside its built-in skill library.
      skills.external_dirs = [ "/var/lib/hermes/custom-skills" ];
    };
    # Non-secret env vars (bot allowlist + provider endpoint). Secrets go via environmentFiles.
    environment = {
      TELEGRAM_ALLOWED_USERS = "5368588092,5369058954";
      XIAOMI_BASE_URL = "https://token-plan-cn.xiaomimimo.com/v1";
    };
    environmentFiles = [ config.sops.templates."hermes-env".path ];
  };

  # S3 FUSE mount for iDrive e2 docs bucket
  fileSystems."/mnt/s3/docs" = {
    device = "docs";
    fsType = "fuse./run/current-system/sw/bin/s3fs";
    noCheck = true;
    options = [
      "_netdev"
      "allow_other"
      "use_path_request_style"
      "url=https://s3.us-west-1.idrivee2.com"
      "endpoint=us-west-1"
      "passwd_file=${config.sops.templates."s3fs-passwd".path}"
    ];
  };
}
