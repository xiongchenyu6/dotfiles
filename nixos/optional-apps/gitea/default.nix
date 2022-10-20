# Edit this configuration file to define what should be installed on

# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

let
  common-files-path = ../../../common;
  secret-files-path = common-files-path + "/secrets";
in {
  age.secrets.gitea_postgres = {
    file = secret-files-path + /gitea_postgres.age;
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
        passwordFile = config.age.secrets.gitea_postgres.path;
        createDatabase = false;
      };
      settings = { server = { SSH_PORT = 2222; }; };
      extraConfig = ''
        [database]
        SSL_MODE = require
      '';
      # extraConfig = ''
      #   [database]
      #   SSL_MODE = skip-verify
      # '';
    };
  };
}
