{
  services.restic = {
    server = {
      enable = false;
      listenAddress = "localhost:8001";
    };
  };
}
