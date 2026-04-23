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

    # Upstream package.nix is stale after hashtopolis renamed *.class.php → *.php
    # and bumped composer deps. Two fixes layered on top of pkgs.hashtopolis-server:
    #   1. composerVendor: relax composerStrictValidation (lockfile drift) and
    #      pin the updated vendor hash.
    #   2. postPatch: point substituteInPlace at the new filenames.
    # Remove once xiongchenyu6/nur-packages ships a refreshed package.nix.
    package = pkgs.hashtopolis-server.overrideAttrs (_: previousAttrs: {
      composerVendor = previousAttrs.composerVendor.overrideAttrs (_: {
        composerStrictValidation = false;
        outputHash = "sha256-KobiZlzJjL6nOsxmbndnNxE5a9ElLvU+ehdnvcRqtxo=";
      });
      postPatch = ''
        substituteInPlace src/inc/utils/Lock.php \
          --replace-fail 'dirname(__FILE__) . "/locks/"' '(getenv("HASHTOPOLIS_LOCKS_PATH") ?: dirname(__FILE__) . "/locks/") . "/"'
        substituteInPlace src/inc/utils/LockUtils.php \
          --replace-fail 'dirname(__FILE__) . "/locks/"' '(getenv("HASHTOPOLIS_LOCKS_PATH") ?: dirname(__FILE__) . "/locks/") . "/"'
        sed -i '2a require_once(dirname(__FILE__) . "/../../vendor/autoload.php");' src/install/updates/update.php
      '';
    });

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
      virtualHost = "hashtopolis.panda.qzz.io";
    };
  };

  # Configure nginx SSL for hashtopolis virtual host
  services.nginx.virtualHosts."hashtopolis.panda.qzz.io" = {
    forceSSL = true;
    enableACME = true;
  };

  # MariaDB client for database management
  environment.systemPackages = [ pkgs.mariadb ];
}
