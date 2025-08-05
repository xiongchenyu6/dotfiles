{ config, pkgs, ... }:

{
  imports = [
    ../../nixos-modules/dnf-server-native.nix
  ];

  # Enable native DNF server with OCI container
  services.dnf-server-native = {
    enable = true;
    publicIP = "203.117.139.182"; # Game server's public IP
    mysqlHost = "host.containers.internal"; # Use host network from container
    mysqlPort = 3000;
    rootPassword = "88888888";
    gmAccount = "gmuser";
    gmPassword = "gmpass";
    serverGroup = 3;
    openChannels = "11,52";
    backend = "podman"; # Use Podman as container backend
  };

  # Use Podman instead of Docker
  virtualisation.podman = {
    enable = true;
    dockerCompat = true; # Create docker alias for compatibility
    defaultNetwork.settings.dns_enabled = true;
  };

  # Convenience packages
  environment.systemPackages = with pkgs; [
    podman
    podman-compose
    htop # Process monitoring
    iotop # I/O monitoring
    nethogs # Network monitoring
  ];

  # Convenience aliases for management
  environment.shellAliases = {
    dnf-status = "podman ps | grep -E 'mysql|dnf-1'";
    dnf-logs = "podman logs dnf-1 -f";
    dnf-logs-mysql = "podman logs mysql -f";
    dnf-mysql = "mysql -h 127.0.0.1 -P 3000 -u root -p88888888";
    dnf-shell = "podman exec -it dnf-1 /bin/bash";
    dnf-restart = "systemctl restart podman-mysql && systemctl restart podman-dnf-1";
    dnf-supervisor = "xdg-open http://localhost:2000"; # Open supervisor web interface
  };
}
