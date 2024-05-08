{ pkgs, config, ... }: {
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
      package =
        pkgs.datadog-agent.override { extraTags = [ "docker" "podman" ]; };
      hostname = config.networking.hostName;
      site = "datadoghq.com";
      apiKeyFile = config.sops.secrets."datadog".path;
      enableTraceAgent = true;
      networkCheck = {
        instances = [{
          collect_connection_state = true;
          excluded_interfaces = [ "lo" "lo0" ];
        }];
      };
      enableLiveProcessCollection = true;
      extraIntegrations = {
        btrfs = ps: [ ps.psutil ];
        journald = ps: [ ps.psutil ];
      };
      checks = { "journald.d" = { logs = [{ type = "journald"; }]; }; };
      extraConfig = { logs_enabled = true; };
    };
  };
}
