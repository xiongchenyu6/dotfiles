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
        log_format nginx '\$remote_addr - \$remote_user [\$time_local] '
                        '"\$request" \$status \$body_bytes_sent \$request_time '
                        '"\$http_referer" "\$http_user_agent"';

        access_log /var/log/nginx/access.log;
        error_log /var/log/nginx/error.log;
      '';
    };
    # prometheus.exporters = {
    #   nginx = {
    #     enable = true;
    #     port = 9005;
    #   };
    # };
    logrotate = {
      settings = {
        nginx = {
          files = [ "/var/log/nginx/*.log" ];
          frequency = "daily";
        };
      };
    };
  };
}
