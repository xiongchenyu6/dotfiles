# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ config, lib, ... }: {
  sops.secrets = {
    "django/secret" = {
      mode = "777";
      owner = "healthchecks";
      group = "healthchecks";
    };
  };

  services = {
    healthchecks = {
      port = 8001;
      listenAddress = "${config.networking.fqdn}";
      enable = true;
      settings = {
        DEBUG = true;
        SECRET_KEY_FILE = config.sops.secrets."django/secret".path;
        COMPRESS_ENABLED = "False";
        REGISTRATION_OPEN = true;
        EMAIL_HOST = "mail.freeman.engineer";
        EMAIL_HOST_PASSWORD = "0";
        EMAIL_HOST_USER = "freeman";
        DEFAULT_FROM_EMAIL = "healthchecks@mail.freeman.engineer";
        PING_ENDPOINT = "https://healthchecks.inner.freeman.engineer/";
        SITE_ROOT = "https://healthchekcs.inner.${config.networking.domain}";
      };
    };
    nginx = {
      virtualHosts = {
        healthchecks = {
          serverName = "healthchecks.inner.${config.networking.domain}";
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "inner.${config.networking.domain}";
          kTLS = true;
          locations."/" = {
            proxyPass =
              "http://127.0.0.1:${toString config.services.healthchecks.port}";
          };
        };
      };
    };
  };

  systemd.services = {
    healthchecks = let
      cfg = config.services.healthchecks;
      pkg = cfg.package;
    in {
      preStart = lib.mkForce ''
        ${pkg}/opt/healthchecks/manage.py collectstatic --no-input
        ${pkg}/opt/healthchecks/manage.py remove_stale_contenttypes --no-input
        ${pkg}/opt/healthchecks/manage.py compress --force
      '';
    };
  };
}
