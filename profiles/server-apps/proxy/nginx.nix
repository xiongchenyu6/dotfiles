{
  config,
  pkgs,
  lib,
  ...
}: {
  services = {
    nginx = {
      enable = true;
      # statusPage = true;
      recommendedProxySettings = true;
      gitweb = {enable = true;};
      additionalModules = [pkgs.nginxModules.pam];

      virtualHosts = {
        bird-lg = {
          serverName = "bird-lg.inner.${config.networking.domain}";
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "inner.${config.networking.domain}";
          kTLS = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:5000";
            proxyWebsockets = true;
          };
        };
        grafana = {
          serverName = "grafana.inner.${config.networking.domain}";
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "inner.${config.networking.domain}";
          kTLS = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:${
              toString config.services.grafana.settings.server.http_port
            }";
            proxyWebsockets = true;
          };
        };
        hydra = {
          serverName = "hydra.inner.${config.networking.domain}";
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "inner.${config.networking.domain}";
          kTLS = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:3000";
            proxyWebsockets = true;
          };
        };
        prometheus = {
          serverName = "prometheus.inner.${config.networking.domain}";
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "inner.${config.networking.domain}";
          kTLS = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:9090";
            proxyWebsockets = true;
            extraConfig = ''
              auth_pam  "Password Required";
              auth_pam_service_name "nginx";
            '';
          };
        };
        alps = {
          serverName = "alps.inner.${config.networking.domain}";
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "inner.${config.networking.domain}";
          kTLS = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:1323";
            proxyWebsockets = true;
          };
        };
        gitea = {
          serverName = "git.inner.${config.networking.domain}";
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "inner.${config.networking.domain}";
          kTLS = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:${toString config.services.gitea.httpPort}";
            proxyWebsockets = true;
          };
        };

        healthchecks = {
          serverName = "healthchecks.inner.${config.networking.domain}";
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "inner.${config.networking.domain}";
          kTLS = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:${toString config.services.healthchecks.port}";
          };
        };
        calibre-web = {
          serverName = "calibre-web.inner.${config.networking.domain}";
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "inner.${config.networking.domain}";
          kTLS = true;
          locations."/" = {
            proxyPass = "http://localhost:${
              toString config.services.calibre-web.listen.port
            }";
          };
        };
        gotify = {
          serverName = "gotify.inner.${config.networking.domain}";
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "inner.${config.networking.domain}";
          kTLS = true;
          locations."/" = {
            proxyPass = "http://localhost:${toString config.services.gotify.port}";
          };
        };
        zammad = {
          serverName = "zammad.inner.${config.networking.domain}";
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "inner.${config.networking.domain}";
          kTLS = true;
          locations."/" = {
            proxyPass = "http://localhost:${toString config.services.zammad.port}";
          };
        };

        mail = {
          # forceSSL = lib.mkForce true;
          onlySSL = true;
          acmeRoot = null;
          useACMEHost = "inner.${config.networking.domain}";
          kTLS = true;
          serverName = "discourse.inner.${config.networking.domain}";
        };
      };
    };
  };

  systemd.services.nginx.serviceConfig = {
    SupplementaryGroups = ["shadow"];
    NoNewPrivileges = lib.mkForce false;
    PrivateDevices = lib.mkForce false;
    ProtectHostname = lib.mkForce false;
    ProtectKernelTunables = lib.mkForce false;
    ProtectKernelModules = lib.mkForce false;
    RestrictAddressFamilies = lib.mkForce [];
    LockPersonality = lib.mkForce false;
    MemoryDenyWriteExecute = lib.mkForce false;
    RestrictRealtime = lib.mkForce false;
    RestrictSUIDSGID = lib.mkForce false;
    SystemCallArchitectures = lib.mkForce "";
    ProtectClock = lib.mkForce false;
    ProtectKernelLogs = lib.mkForce false;
    RestrictNamespaces = lib.mkForce false;
    SystemCallFilter = lib.mkForce "";
  };
}
