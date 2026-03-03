{
  config,
  inputs,
  modulesPath,
  lib,
  pkgs,
  ezModules,
  ...
}:
let
  zeroclawPkgs = inputs.zeroclaw.packages.${pkgs.system};
  # Fix upstream stale npm deps hash in zeroclaw-web (FOD hash mismatch)
  zeroclaw-web-fixed = zeroclawPkgs.zeroclaw-web.overrideAttrs (old: {
    npmDepsHash = "sha256-+F9yjRj5QLnyFrRFabIhEyyc02AFXVPN+p4q+EvEhGI=";
    npmDeps = pkgs.fetchNpmDeps {
      src = old.src;
      hash = "sha256-+F9yjRj5QLnyFrRFabIhEyyc02AFXVPN+p4q+EvEhGI=";
    };
  });
  zeroclaw =
    (zeroclawPkgs.zeroclaw.override {
      zeroclaw-web = zeroclaw-web-fixed;
    }).overrideAttrs
      (old: {
        # Upstream package.nix fileset is incomplete (missing build.rs, templates/, etc.)
        # Use full source from the flake instead
        src = inputs.zeroclaw;
        prePatch = ''
          mkdir -p web
          ln -sf ${zeroclaw-web-fixed} web/dist
        '';
      });
in
{
  imports = with inputs; [
    xiongchenyu6.nixosModules.casdoor
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
    ./hardware-configuration.nix
  ];

  sops.secrets."cloudflared/tunnel-credentials" = { };

  environment = {
    systemPackages = [
      pkgs.cloudflared
      zeroclaw
    ];
  };

  networking = {
    firewall = {
      allowedTCPPorts = [
        80
        443
        636
      ];
      allowedUDPPorts = [ 53 ]; # Added for DNS
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

  services = {
    postgresql = {
      enable = true;
      package = pkgs.postgresql_18_jit;
      authentication = ''
        local all all trust
        host  all  all 127.0.0.1/32 trust
        host  all  all ::1/128 trust
        host  all  all 0.0.0.0/0 scram-sha-256
      '';
      enableJIT = true;
      enableTCPIP = true;
      settings = {
        log_connections = true;
        log_statement = "all";
        logging_collector = true;
        log_disconnections = true;
        log_destination = lib.mkForce "syslog";
      };
      ensureUsers = [
        {
          name = "casdoor";
          ensureDBOwnership = true;
        }
      ];
      ensureDatabases = [
        "casdoor"
      ];
    };

    nginx = {
      virtualHosts = {
        "casdoor.${config.networking.domain}" = {
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "ai";
          kTLS = true;
          locations = {
            "/" = {
              proxyPass = "http://127.0.0.1:8000";
              proxyWebsockets = true;
            };
          };
        };
      };
    };
  };

  # ZeroClaw AI Gateway - Rust-based, <5MB RAM
  # Config is written to /var/lib/zeroclaw/.zeroclaw/config.toml
  systemd.services.zeroclaw = {
    description = "ZeroClaw AI Gateway (Telegram + NVIDIA NIM + Brave Search)";
    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    environment = {
      HOME = "/var/lib/zeroclaw";
      RUST_LOG = "info";
    };
    serviceConfig = {
      ExecStartPre =
        let
          configFile = pkgs.writeText "zeroclaw-config.toml" ''
            # ZeroClaw config for oracle-amd-001
            # Provider: NVIDIA NIM (Kimi K2.5) via OpenAI-compatible API
            default_provider = "custom:https://integrate.api.nvidia.com/v1"
            api_key = "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"
            default_model = "moonshotai/kimi-k2.5"
            default_temperature = 0.7

            # Web Search via Brave
            [web_search]
            enabled = true
            provider = "brave"
            brave_api_key = "BSAfZcxnhk7D-O0urvk6ygSSciDQpNv"
            max_results = 5

            # Telegram Channel
            [channels_config]
            cli = false

            [channels_config.telegram]
            bot_token = "8759926301:AAG5dAb9htQc_Fo_FKwNVS5BZaB-Ks1ca0w"
            allowed_users = ["5368588092"]
            stream_mode = "partial"
            ack_enabled = true
            interrupt_on_new_message = true

            [channels_config.telegram.group_reply]
            mode = "mention_only"
            allowed_sender_ids = []
          '';
        in
        "${pkgs.coreutils}/bin/install -D -m 600 ${configFile} /var/lib/zeroclaw/.zeroclaw/config.toml";
      ExecStart = "${zeroclaw}/bin/zeroclaw daemon";
      Restart = "always";
      RestartSec = 5;
      User = "zeroclaw";
      Group = "zeroclaw";
      WorkingDirectory = "/var/lib/zeroclaw";
      StateDirectory = "zeroclaw";
      # Security hardening
      ProtectSystem = "strict";
      ProtectHome = true;
      PrivateTmp = true;
      NoNewPrivileges = true;
      ReadWritePaths = [ "/var/lib/zeroclaw" ];
    };
  };

  users.users.zeroclaw = {
    isSystemUser = true;
    group = "zeroclaw";
    home = "/var/lib/zeroclaw";
    createHome = true;
  };
  users.groups.zeroclaw = { };
}
