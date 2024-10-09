{
  services = {
    loki = let configFile = ./complete-local-config.yaml;
    in {
      enable = true;
      inherit configFile;
    };
  };
}
