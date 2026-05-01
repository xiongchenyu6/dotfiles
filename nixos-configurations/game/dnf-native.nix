{ config, pkgs, lib, inputs, ... }:

{
  imports = [
    inputs.xiongchenyu6.nixosModules.dnf-server
  ];

  # Sops secrets for DNF server
  sops.secrets."dnf/root_password" = { };
  sops.secrets."dnf/gm_password" = { };
  sops.secrets."dnf/gm_connect_key" = { };
  sops.secrets."dnf/game_password" = { };

  # Use real Docker for OCI containers on this host. Podman's signal/SIGCHLD
  # propagation triggers EINTR in old libmysqlclient inside the dnf-1 image,
  # crash-looping tongyi_gate. Disable podman's docker shims so the real
  # docker CLI/daemon owns the `docker` command and /var/run/docker.sock.
  virtualisation = {
    docker.enable = true;
    podman = {
      dockerCompat = lib.mkForce false;
      dockerSocket.enable = lib.mkForce false;
    };
  };

  # Per upstream 1995chen/dnf FAQ: 统一登陆器5.x requires start.dnf.tw to
  # resolve. Without this it hits a real public IP that doesn't speak the
  # private-server protocol and the launcher hangs after login.
  networking.hosts = {
    "127.0.0.1" = [ "start.dnf.tw" ];
  };

  # Enable native DNF server with OCI container
  services.dnf-server-native = {
    enable = true;
    publicIP = "127.0.0.1"; # local-only play; docker port-forwards all game ports
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
    backend = "docker";
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
    dnf-restart = "systemctl restart docker-mysql && systemctl restart docker-dnf-1";
    dnf-supervisor = "xdg-open http://localhost:2000"; # Open supervisor web interface
  };
}
