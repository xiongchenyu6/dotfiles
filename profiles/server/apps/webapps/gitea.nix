# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ config, lib, ... }: {
  sops.secrets."database/postgres/gitea" = {
    mode = "770";
    owner = "gitea";
    group = "gitea";
  };

  services = {
    gitea = {
      enable = true;
      lfs.enable = true;
      rootUrl = "https://git.inner.${config.networking.domain}";
      httpPort = 3002;
      database = {
        user = "gitea";
        name = "gitea";
        # host = "mysqlfornixos.mysql.database.azure.com";
        # type = "mysql";
        host = "postgres-database-1.postgres.database.azure.com";
        type = "postgres";
        passwordFile = config.sops.secrets."database/postgres/gitea".path;
        createDatabase = false;
      };
      settings = {
        server = { SSH_PORT = 2222; };
        database = { SSL_MODE = lib.mkForce "require"; };
        "cron.sync_external_users" = { RUN_AT_START = true; };
      };
      # extraConfig = ''
      #   [database]
      #   SSL_MODE = require
      #   [cron.sync_external_users]
      #   RUN_AT_START = true
      # '';
      # extraConfig = ''
      #   [database]
      #   SSL_MODE = skip-verify
      # '';
    };
    nginx = {
      virtualHosts = {
        gitea = {
          serverName = "git.inner.${config.networking.domain}";
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "${config.networking.domain}";
          kTLS = true;
          locations."/" = {
            proxyPass =
              "http://127.0.0.1:${toString config.services.gitea.httpPort}";
            proxyWebsockets = true;
          };
        };
      };
    };
  };
}
