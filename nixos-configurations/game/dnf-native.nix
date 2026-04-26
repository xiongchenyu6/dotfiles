{ config, pkgs, inputs, ... }:

{
  imports = [
    inputs.xiongchenyu6.nixosModules.dnf-server
  ];

  # Sops secrets for DNF server
  sops.secrets."dnf/root_password" = { };
  sops.secrets."dnf/gm_password" = { };
  sops.secrets."dnf/gm_connect_key" = { };
  sops.secrets."dnf/game_password" = { };

  # Enable native DNF server with OCI container
  services.dnf-server-native = {
    enable = true;
    publicIP = "192.168.15.210"; # LAN IP (wlp4s0)
    mysqlHost = "host.containers.internal"; # Use host network from container
    mysqlPort = 3000;
    rootPasswordFile = config.sops.secrets."dnf/root_password".path;
    gmAccount = "gmuser";
    gmPasswordFile = config.sops.secrets."dnf/gm_password".path;
    gmConnectKeyFile = config.sops.secrets."dnf/gm_connect_key".path;
    gamePasswordFile = config.sops.secrets."dnf/game_password".path;
    serverGroup = 3;
    serverGroupDB = "cain"; # README recommends cain even for siroco
    clientPoolSize = 10;
    openChannels = "11,52";
    backend = "podman"; # Podman is enabled in nixos-modules/virtualisation.nix
  };

  # Convenience packages
  environment.systemPackages = with pkgs; [
    htop # Process monitoring
    iotop # I/O monitoring
    nethogs # Network monitoring
  ];

  # Convenience aliases for management
  environment.shellAliases = {
    dnf-status = "docker ps | grep -E 'mysql|dnf-1'";
    dnf-logs = "docker logs dnf-1 -f";
    dnf-logs-mysql = "docker logs mysql -f";
    dnf-mysql = "mysql -h 127.0.0.1 -P 3000 -u root -p";
    dnf-shell = "docker exec -it dnf-1 /bin/bash";
    dnf-restart = "systemctl restart podman-mysql && systemctl restart podman-dnf-1";
    dnf-supervisor = "xdg-open http://localhost:2000"; # Open supervisor web interface
  };
}
