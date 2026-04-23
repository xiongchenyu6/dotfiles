{
  config,
  inputs,
  ...
}:

{
  # Dify secrets
  sops.secrets."dify/secret_key" = {
    owner = "dify";
  };

  # Import Dify NixOS module from NUR packages
  imports = [
    inputs.xiongchenyu6.nixosModules.dify
  ];

  services.dify = {
    enable = true;

    # Packages from the NUR overlay
    package = {
      api = inputs.xiongchenyu6.packages.aarch64-linux.dify-api;
      web = inputs.xiongchenyu6.packages.aarch64-linux.dify-web;
    };

    secretKeyFile = config.sops.secrets."dify/secret_key".path;

    api = {
      host = "127.0.0.1";
      port = 5002;
      workers = 2;
      logLevel = "INFO";
      migrationEnabled = true;
      consoleApiUrl = "https://dify.panda.qzz.io";
      serviceApiUrl = "https://dify.panda.qzz.io";
      appApiUrl = "https://dify.panda.qzz.io";
      filesUrl = "https://dify.panda.qzz.io";
    };

    web = {
      port = 3000;
      consoleWebUrl = "https://dify.panda.qzz.io";
    };

    database = {
      createLocally = true;
      enablePgvector = true;
      host = "127.0.0.1";
    };

    redis = {
      createLocally = true;
    };

    storage = {
      type = "local";
    };

    nginx = {
      enable = true;
      domain = "dify.panda.qzz.io";
    };
  };

  # SSL for dify virtual host
  services.nginx.virtualHosts."dify.panda.qzz.io" = {
    forceSSL = true;
    enableACME = true;
  };
}
