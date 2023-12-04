{ config, ... }: {
  sops.secrets = {
    "database/postgres/keycloak" = {
      mode = "770";
      owner = "postgres";
      group = "postgres";
    };
  };

  services = {
    keycloak = let
      credsDir =
        config.security.acme.certs."${config.networking.fqdn}".directory;
      sslCert = credsDir + "/cert.pem";
      sslKey = credsDir + "/key.pem";
    in {
      enable = true;
      sslCertificate = sslCert;
      sslCertificateKey = sslKey;
      settings = {
        hostname = "keycloak.inner.${config.networking.domain}";
        http-port = 8234;
        https-port = 8235;
        http-host = "127.0.0.1";
      };
      database = {
        passwordFile = config.sops.secrets."database/postgres/keycloak".path;
      };
    };
    nginx = {
      virtualHosts = {
        keycloak = {
          serverName = "keycloak.inner.${config.networking.domain}";
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "inner.${config.networking.domain}";
          kTLS = true;
          locations."/" = {
            proxyPass = "https://localhost:${
                toString config.services.keycloak.settings.https-port
              }";
          };
        };
      };
    };
  };
}
