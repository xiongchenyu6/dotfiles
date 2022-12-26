{ config, ... }: {
  services = {
    promtail = {
      enable = true;
      configuration = {
        server = {
          http_listen_port = 9080;
          grpc_listen_port = 0;
        };
        clients = [{
          url = "http://mail.${config.networking.domain}:3100/api/prom/push";
        }];
        scrape_configs = [{
          job_name = "systemd-journal";
          journal = {
            max_age = "12h";
            path = "/var/log/journal";
          };
          relabel_configs = [{
            source_labels = [ "__journal__systemd_unit" ];
            target_label = "unit";
          }];
        }];
      };
    };
  };
}

