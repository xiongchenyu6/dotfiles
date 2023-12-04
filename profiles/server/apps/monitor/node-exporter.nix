{
  services.prometheus.exporters = {
    node = {
      enabledCollectors = [ "systemd" ];
      enable = true;
      port = 9002;
    };
  };
}
