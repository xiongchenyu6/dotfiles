{ pkgs, lib, ... }: {
  services.prometheus = {
    enable = true;
    extraFlags = [
      "--web.console.libraries ${pkgs.prometheus}/etc/prometheus/console_libraries"
      "--web.console.templates ${pkgs.prometheus}/etc/prometheus/consoles"
    ];
    scrapeConfigs = [{
      job_name = "node";
      static_configs = [{
        targets = [
          "localhost:9002"
          "localhost:9003"
          "localhost:9004"
          "localhost:9005"
          "localhost:9007"
          "localhost:9008"
          "localhost:9009"
          "localhost:9010"
          "localhost:9119"
        ];
      }];
    }];
  };
  systemd.services.prometheus.serviceConfig = {
    SystemCallFilter = lib.mkForce [ ];
  };
}
