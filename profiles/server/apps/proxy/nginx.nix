{ pkgs, lib, ... }:
{
  services = {
    nginx = {
      enable = true;
      statusPage = true;
      recommendedProxySettings = true;
      #      gitweb = { enable = true; };
      additionalModules = [ pkgs.nginxModules.pam ];
      eventsConfig = ''
        worker_connections 20000;
      '';
      appendHttpConfig = ''
        include ${./log};
        error_log syslog:server=unix:/dev/log;
        access_log syslog:server=unix:/dev/log main;
      '';
    };
    # prometheus.exporters = {
    #   nginx = {
    #     enable = true;
    #     port = 9005;
    #   };
    # };
  };
}
