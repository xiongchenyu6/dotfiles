{ config, ... }: {

  sops.secrets."openldap/credentials" = {
    mode = "770";
    owner = "openldap-exporter";
    group = "openldap-exporter";
  };

  users = {
    users = {
      openldap-exporter = {
        group = "openldap-exporter";
        isSystemUser = true;
      };
    };
    groups.openldap-exporter = { };
  };

  services.prometheus.exporters = {
    node = {
      enabledCollectors = [ "systemd" ];
      enable = true;
      port = 9002;
    };

    bird = {
      enable = true;
      port = 9003;
    };

    domain = {
      enable = true;
      port = 9004;
    };

    nginx = {
      enable = true;
      port = 9005;
    };

    openldap = {
      enable = false;
      port = 9007;
      ldapCredentialFile = config.sops.secrets."openldap/credentials".path;
    };

    postfix = {
      enable = true;
      port = 9008;
    };

    postgres = {
      enable = true;
      port = 9009;
    };

    wireguard = {
      enable = true;
      port = 9010;
    };

  };
}
