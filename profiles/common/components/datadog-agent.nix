{ pkgs, config, ... }:
{
  sops.secrets."datadog" = {
    mode = "777";
    owner = "datadog";
    group = "datadog";
  };

  # systemd.services.datadog-agent.serviceConfig.User = lib.mkForce "root";
  # systemd.services.datadog-agent.serviceConfig.Group = lib.mkForce "root";
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
        };
      };
      extraConfig = {
        logs_enabled = true;
      };
    };
  };
}
