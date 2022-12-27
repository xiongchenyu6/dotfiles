{ config, pkgs, lib, ... }: {
  services = {
    prometheus = {
      enable = true;
      extraFlags = [
        "--web.console.libraries ${pkgs.prometheus}/etc/prometheus/console_libraries"
        "--web.console.templates ${pkgs.prometheus}/etc/prometheus/consoles"
      ];
      scrapeConfigs = [{
        job_name = "node";
        static_configs = [{
          targets = [
            "localhost:9002"
            "localhost:9003"
            "localhost:9004"
            "localhost:9005"
            "localhost:9007"
            "localhost:9008"
            "localhost:9009"
            "localhost:9010"
            "localhost:9119"
          ];
        }];
      }];
      exporters = {
        domain = {
          enable = true;
          port = 9004;
        };
      };
    };
    nginx = {
      virtualHosts = {
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
      };
    };
  };

  systemd.services.prometheus.serviceConfig = {
    SystemCallFilter = lib.mkForce [ ];
  };
}
