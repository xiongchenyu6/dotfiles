{ config, pkgs, ... }: {
  sops.secrets = {
    "database/postgres/discourse" = {
      mode = "770";
      owner = "discourse";
      group = "discourse";
    };
    "django/secret" = {
      mode = "777";
      owner = "discourse";
      group = "discourse";
    };
    "openldap/pass/user" = {
      mode = "770";
      owner = "discourse";
      group = "discourse";
    };
  };

  services = {
    discourse = {
      enable = true;
      hostname = "discourse.inner.${config.networking.domain}";
      package = pkgs.discourseAllPlugins;
      plugins = with config.services.discourse.package.plugins; [
        discourse-ldap-auth
        discourse-solved
        discourse-spoiler-alert
        discourse-voting
      ];
      enableACME = true;
      # secretKeyBaseFile = config.age.secrets.django_secret.path;
      database = {
        host = "postgres-database-1.postgres.database.azure.com";
        name = "discourse";
        username = "discourse";
        passwordFile = config.sops.secrets."database/postgres/discourse".path;
        ignorePostgresqlVersion = true;
      };
      admin = {
        fullName = "freeman.xiong";
        username = "freeman";
        email = "freeman@freeman.engineer";
        passwordFile = config.sops.secrets."django/secret".path;
      };
      mail = {
        incoming = { };
        outgoing = {
          serverAddress = "${config.networking.fqdn}";
          username = "discourse";
          passwordFile = config.sops.secrets."openldap/pass/user".path;
          domain = "${config.networking.fqdn}";
          authentication = "plain";
        };
      };
      nginx.enable = true;
    };
    nginx = {
      virtualHosts = {
        # use system hostname to match the implicit nginx config
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
}
