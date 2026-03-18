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
      consoleApiUrl = "https://dify.xiongchenyu.dpdns.org";
      serviceApiUrl = "https://dify.xiongchenyu.dpdns.org";
      appApiUrl = "https://dify.xiongchenyu.dpdns.org";
      filesUrl = "https://dify.xiongchenyu.dpdns.org";
    };

    web = {
      port = 3000;
      consoleWebUrl = "https://dify.xiongchenyu.dpdns.org";
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
      domain = "dify.xiongchenyu.dpdns.org";
    };
  };

  # SSL for dify virtual host
  services.nginx.virtualHosts."dify.xiongchenyu.dpdns.org" = {
    forceSSL = true;
    enableACME = true;
  };
}
