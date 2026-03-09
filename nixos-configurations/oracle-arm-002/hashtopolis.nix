{
  config,
  pkgs,
  ...
}:

{
  # Hashtopolis server configuration for oracle-arm-002
  sops.secrets."hashtopolis/admin_password" = { };
  sops.secrets."hashtopolis/db_password" = { };

  services.hashtopolis-server = {
    enable = true;

    listenAddress = "0.0.0.0";
    port = 8080;
    dataDir = "/var/lib/hashtopolis";

    adminUser = "admin";
    adminPasswordFile = config.sops.secrets."hashtopolis/admin_password".path;

    database = {
      createLocally = true;
      host = "localhost";
      port = 3306;
      name = "hashtopolis";
      user = "hashtopolis";
      passwordFile = config.sops.secrets."hashtopolis/db_password".path;
    };

    # Increase PHP memory for large task pages
    phpOptions.memory_limit = "1024M";

    nginx = {
      enable = true;
      virtualHost = "hashtopolis.xiongchenyu.dpdns.org";
    };
  };

  # Configure nginx SSL for hashtopolis virtual host
  services.nginx.virtualHosts."hashtopolis.xiongchenyu.dpdns.org" = {
    forceSSL = true;
    enableACME = true;
  };

  # MariaDB client for database management
  environment.systemPackages = [ pkgs.mariadb ];
}
