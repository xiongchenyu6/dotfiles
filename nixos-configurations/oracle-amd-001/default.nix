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
    systemPackages = with pkgs; [
      cloudflared
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
        password = "casdoor";
        name = "casdoor";
      };
      redis = {
        enable = false;
      };
      staticBaseUrl = "https://cdn.casbin.org";
      autoStart = true;
    };

    cloudflared = {
      enable = true;
      tunnels = {
        "31881776-71bc-4c81-b206-b579ca61ffd2" = {
          credentialsFile = config.sops.secrets."cloudflared/tunnel-credentials".path;
          # Default route for unmatched requests
          warp-routing = {
            enabled = true;
          };
          default = "http_status:404";
          # Add specific routes here as needed, for example:
          # "kanidm.${config.networking.domain}" = "localhost:443";
        };
      };
    };
  };
}
