{ config, pkgs, lib, ... }:

let
  common-files-path = ../../common;
  secret-files-path = common-files-path + "/secrets";

in {
  age.secrets.discourse_postgres = {
    file = secret-files-path + /discourse_postgres.age;
    mode = "770";
    owner = "discourse";
    group = "discourse";
  };

  age.secrets.ldap_user_pass = {
    file = secret-files-path + /ldap_user_pass.age;
    mode = "770";
    owner = "discourse";
    group = "discourse";
  };

  services.discourse = {
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
      passwordFile = config.age.secrets.discourse_postgres.path;
      ignorePostgresqlVersion = true;
    };
    admin = {
      fullName = "freeman.xiong";
      username = "freeman";
      email = "freeman@freeman.engineer";
      passwordFile = config.age.secrets.django_secret.path;
    };
    mail = {
      incoming = { };
      outgoing = {
        serverAddress = "${config.networking.fqdn}";
        username = "discourse";
        passwordFile = config.age.secrets.ldap_user_pass.path;
        domain = "${config.networking.fqdn}";
        authentication = "plain";
      };
    };
    nginx.enable = true;
  };
}

