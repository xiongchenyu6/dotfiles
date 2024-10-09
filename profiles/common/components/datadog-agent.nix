{
  pkgs,
  config,
  lib,
  ...
}:
{
  sops.secrets."datadog" = {
    mode = "777";
    owner = "datadog";
    group = "datadog";
  };

  users.users.datadog.extraGroups = [
    "systemd-journal"
    "networkmanager"
    "wheel"
    "video"
    "audio"
    "cdrom"
    "disk"
    "floppy"
    "dialout"
    "lp"
    "input"
    "docker"
    "podman"
    "tss"
    "libvirtd"
    "pulse"
    "pipewire"
  ];

  services = {
    datadog-agent = {
      enable = true;
      hostname = config.networking.hostName;
      site = "datadoghq.com";
      apiKeyFile = config.sops.secrets."datadog".path;
      enableTraceAgent = true;
      networkCheck = {
        instances = [
          {
            collect_connection_state = true;
            excluded_interfaces = [
              "lo"
              "lo0"
            ];
          }
        ];
      };
      enableLiveProcessCollection = true;
      extraIntegrations = {
        btrfs = ps: [ ps.psutil ];
        journald = ps: [ ps.psutil ];
      };
      checks = {
        "journald" = {
          logs = [ { type = "journald"; } ];
        };
        "nginx" = {
          instances = [ { nginx_status_url = "http://localhost/nginx_status"; } ];
          logs = [
            {
              type = "file";
              path = "/var/log/nginx/access.log";
              service = "nginx";
              source = "nginx";
            }
            {
              type = "file";
              path = "/var/log/nginx/error.log";
              service = "nginx";
              source = "nginx";
            }
          ];
        };
      };
      extraConfig = {
        logs_enabled = true;
      };
    };
  };
}
