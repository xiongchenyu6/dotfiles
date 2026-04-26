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
    xiongchenyu6.nixosModules.casdoor
    xiongchenyu6.nixosModules.openagent
    xiongchenyu6.nixosModules.sub2api
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
    ./hardware-configuration.nix
  ];

  sops.secrets."cloudflared/tunnel-credentials" = { };
  sops.secrets."casdoor/db_password" = { };
  sops.secrets."casibase/client_id" = { };
  sops.secrets."casibase/client_secret" = { };
  sops.secrets."sub2api/env" = {
    owner = "sub2api";
    group = "sub2api";
  };

  systemd.services.sub2api = {
    after = [
      "postgresql.service"
      "redis-sub2api.service"
    ];
    requires = [
      "postgresql.service"
      "redis-sub2api.service"
    ];
  };

  # Inject openagent (formerly casibase) credentials into its config at runtime
  systemd.services.openagent.serviceConfig.ExecStartPre = lib.mkAfter [
    "+${pkgs.writeShellScript "openagent-inject-credentials" ''
      cfg="/var/lib/openagent/conf/app.conf"
      if [ -f "$cfg" ]; then
        client_id=$(cat ${config.sops.secrets."casibase/client_id".path})
        client_secret=$(cat ${config.sops.secrets."casibase/client_secret".path})
        ${pkgs.gnused}/bin/sed -i "s|clientId = .*|clientId = $client_id|" "$cfg"
        ${pkgs.gnused}/bin/sed -i "s|clientSecret = .*|clientSecret = $client_secret|" "$cfg"
      fi
    ''}"
  ];

  # Inject casdoor DB password into its config at runtime
  systemd.services.casdoor.serviceConfig.ExecStartPre = lib.mkAfter [
    "+${pkgs.writeShellScript "casdoor-inject-password" ''
      cfg="/var/lib/casdoor/app.conf"
      if [ -f "$cfg" ]; then
        db_pass=$(cat ${config.sops.secrets."casdoor/db_password".path})
        ${pkgs.gnused}/bin/sed -i "s|dataSourceName = .*|dataSourceName = \"user=casdoor password=$db_pass host=localhost port=5432 dbname=casdoor sslmode=disable\"|" "$cfg"
      fi
    ''}"
  ];

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
        "starslab.qzz.io" = {
          domain = "starslab.qzz.io";
          extraDomainNames = [ "*.starslab.qzz.io" ];
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
        {
          name = "casibase";
          ensureDBOwnership = true;
        }
        {
          name = "sub2api";
          ensureDBOwnership = true;
        }
      ];
      ensureDatabases = [
        "casdoor"
        "casibase"
        "sub2api"
      ];
    };

    casdoor = {
      enable = true;
      appName = "casdoor";
      port = 8000;
      runMode = "prod";
      database = {
        driver = "postgres";
        host = "localhost";
        port = 5432;
        username = "casdoor";
        name = "casdoor";
      };
      redis = {
        enable = false;
      };
      autoStart = true;
    };

    openagent = {
      enable = true;
      port = 14000;
      runMode = "prod";
      database = {
        driver = "postgres";
        host = "localhost";
        port = 5432;
        username = "casibase";
        name = "casibase";
      };
      casdoor = {
        endpoint = "https://casdoor.panda.qzz.io";
        clientId = "PLACEHOLDER_FROM_SOPS";
        clientSecret = "PLACEHOLDER_FROM_SOPS";
        organization = "built-in";
        application = "app-casibase";
      };
      autoStart = true;
    };

    redis.servers.sub2api = {
      enable = true;
      port = 6379;
    };

    sub2api = {
      enable = true;
      port = 18088;
      database = {
        host = "/run/postgresql";
        user = "sub2api";
        name = "sub2api";
      };
      redis = {
        host = "127.0.0.1";
        port = 6379;
      };
      environmentFile = config.sops.secrets."sub2api/env".path;
    };

    nginx = {
      virtualHosts = {
        "casibase.panda.qzz.io" = {
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "panda.qzz.io";
          kTLS = true;
          locations = {
            "/" = {
              proxyPass = "http://127.0.0.1:14000";
              proxyWebsockets = true;
            };
          };
        };
        "casdoor.panda.qzz.io" = {
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "panda.qzz.io";
          kTLS = true;
          locations = {
            "/" = {
              proxyPass = "http://127.0.0.1:8000";
              proxyWebsockets = true;
            };
          };
        };
        "sub2api.starslab.qzz.io" = {
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "starslab.qzz.io";
          kTLS = true;
          locations = {
            "/" = {
              proxyPass = "http://127.0.0.1:18088";
              proxyWebsockets = true;
            };
          };
        };
      };
    };
  };
}
