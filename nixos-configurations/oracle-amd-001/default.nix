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
    ./hardware-configuration.nix
  ];

  # hysteria2 出海入站(UDP 8443):sub2api 跨境 + 个人客户端统一走这个。
  # 历史:裸 VLESS(10086)和 VLESS+Reality(443)都试过并已下线——
  # Reality 同链路只有 83% 成功率,hysteria2 有 93% 且快 3.6 倍。
  my.sing-box-hysteria2.enable = true;

  sops.secrets."cloudflared/tunnel-credentials" = { };

  # Make the already-loaded BBR module effective and allow TCP autotuning to
  # fill high-bandwidth, high-latency paths (nginx 等 TCP 服务仍受益)
  # without forcing every connection to allocate the maximum buffer.
  boot.kernel.sysctl = {
    "net.core.default_qdisc" = "fq";
    "net.ipv4.tcp_congestion_control" = "bbr";
    "net.core.rmem_max" = 33554432;
    "net.core.wmem_max" = 33554432;
    "net.ipv4.tcp_rmem" = "4096 131072 33554432";
    "net.ipv4.tcp_wmem" = "4096 16384 33554432";
  };

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

  environment = {
    systemPackages = [
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
}
