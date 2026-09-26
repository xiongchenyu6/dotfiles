{
  pkgs,
  config,
  lib,
  ...
}:
{
  sops.secrets."datadog" = {
    mode = "0440";
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

  # Agent 7.5x runs live-process collection inside the core agent; the standalone
  # process-agent just logs "process-agent is not enabled, exiting..." and the
  # module's Restart=always turns that into a ~38k/day restart loop.
  systemd.services.datadog-process-agent.enable = false;

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
