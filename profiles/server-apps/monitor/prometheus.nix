{ config, pkgs, ... }: {

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

  services.prometheus = {
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
