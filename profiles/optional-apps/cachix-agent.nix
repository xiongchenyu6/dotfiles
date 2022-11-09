{
  sops.secrets."cachix/secret" = { };

  services = {
    cachix-agent = {
      enable = true;
      credentialsFile = config.sops.secrets."cachix/secret".path;
      verbose = true;
    };
  };
}

