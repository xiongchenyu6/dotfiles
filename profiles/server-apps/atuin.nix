{ config, ... }: {
  services = {
    atuin = {
      enable = true;
      host = "0.0.0.0";
      openRegistration = true;
      openFirewall = true;
    };
    nginx = {
      virtualHosts = {
        atuin = {
          serverName = "atuin.inner.${config.networking.domain}";
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "inner.${config.networking.domain}";
          kTLS = true;
          locations."/" = {
            proxyPass =
              "http://127.0.0.1:${toString config.services.atuin.port}";
            proxyWebsockets = true;
          };
        };
      };
    };
  };
}
