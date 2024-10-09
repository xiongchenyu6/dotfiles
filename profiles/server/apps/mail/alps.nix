{ config, pkgs, lib, ... }: {
  services = {
    alps = {
      enable = true;
      smtps = { port = 465; };
      imaps = { host = config.networking.fqdn; };
    };
    nginx = {
      virtualHosts = {
        alps = {
          serverName = "alps.inner.${config.networking.domain}";
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "${config.networking.domain}";
          kTLS = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:1323";
            proxyWebsockets = true;
          };
        };
      };
    };
  };
  systemd.services.alps = {
    serviceConfig = let cfg = config.services.alps;
    in {
      ExecStart = lib.mkForce ''
        ${pkgs.alps}/bin/alps \
          -addr ${cfg.bindIP}:${toString cfg.port} \
          -theme ${cfg.theme} \
          imaps://${cfg.imaps.host}:${toString cfg.imaps.port} \
          smtps://${cfg.smtps.host}:${toString cfg.smtps.port}
      '';
    };
  };
}
