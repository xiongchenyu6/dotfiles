{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.dnf-server-native;
in
{
  options.services.dnf-server-native = {
    enable = mkEnableOption "Native DNF (Dungeon & Fighter) Server";

    publicIP = mkOption {
      type = types.str;
      default = "203.116.47.202";
      description = "Public IP address for the server";
    };

    mysqlPort = mkOption {
      type = types.port;
      default = 3000;
      description = "MySQL port";
    };

    mysqlHost = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = "MySQL host address";
    };

    rootPassword = mkOption {
      type = types.str;
      default = "PLACEHOLDER";
      description = "MySQL root password (use rootPasswordFile instead for secrets)";
    };

    rootPasswordFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "File containing MySQL root password (overrides rootPassword)";
    };

    gmAccount = mkOption {
      type = types.str;
      default = "gmuser";
      description = "GM account name";
    };

    gmPassword = mkOption {
      type = types.str;
      default = "PLACEHOLDER";
      description = "GM password (use gmPasswordFile instead for secrets)";
    };

    gmPasswordFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "File containing GM password (overrides gmPassword)";
    };

    gmConnectKey = mkOption {
      type = types.str;
      default = "PLACEHOLDER";
      description = "GM connect key (use gmConnectKeyFile instead for secrets)";
    };

    gmConnectKeyFile = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "File containing GM connect key (overrides gmConnectKey)";
    };

    gmLanderVersion = mkOption {
      type = types.str;
      default = "20180307";
      description = "GM lander version";
    };

    serverGroup = mkOption {
      type = types.int;
      default = 3;
      description = "Server group number";
    };

    openChannels = mkOption {
      type = types.str;
      default = "11,52";
      description = "Comma-separated list of channels to open";
    };

    dataDir = mkOption {
      type = types.path;
      default = "/var/lib/dnf-server";
      description = "Data directory for DNF server";
    };

    backend = mkOption {
      type = types.enum [
        "docker"
        "podman"
      ];
      default = "podman";
      description = "Container backend to use";
    };
  };

  config = mkIf cfg.enable {

    # Effective values: use file content if *File is set, otherwise use inline option
    # The env file is generated at runtime by create-dnf-env service
    systemd.tmpfiles.rules = [
      "d /run/dnf-server 0750 root root -"
      "d ${cfg.dataDir} 0755 root root -"
      "d ${cfg.dataDir}/data 0755 root root -"
      "d ${cfg.dataDir}/log 0755 root root -"
      "d ${cfg.dataDir}/mysql 0755 root root -"
    ];

    # Generate env file from sops secrets at runtime
    systemd.services."create-dnf-env" = {
      description = "Generate DNF server environment file from secrets";
      after = [ "sops-nix.service" ];
      before = [
        "${cfg.backend}-mysql.service"
        "${cfg.backend}-dnf-1.service"
      ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
      };
      script = ''
        ROOT_PASS="${cfg.rootPassword}"
        GM_PASS="${cfg.gmPassword}"
        GM_KEY="${cfg.gmConnectKey}"
        ${optionalString (cfg.rootPasswordFile != null) ''ROOT_PASS=$(cat "${cfg.rootPasswordFile}")''}
        ${optionalString (cfg.gmPasswordFile != null) ''GM_PASS=$(cat "${cfg.gmPasswordFile}")''}
        ${optionalString (cfg.gmConnectKeyFile != null) ''GM_KEY=$(cat "${cfg.gmConnectKeyFile}")''}
        cat > /run/dnf-server/env <<EOF
        DNF_DB_ROOT_PASSWORD=$ROOT_PASS
        GM_PASSWORD=$GM_PASS
        GM_CONNECT_KEY=$GM_KEY
        EOF
        chmod 0400 /run/dnf-server/env
      '';
    };

    # Create network for containers (run as root to match container runtime)
    systemd.services."create-dnf-network" = {
      description = "Create DNF container network";
      after = [ "network.target" ];
      before = [
        "${cfg.backend}-mysql.service"
        "${cfg.backend}-dnf-1.service"
      ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        User = "root";
        ExecStart =
          let
            cmd = if cfg.backend == "podman" then "${pkgs.podman}/bin/podman" else "${pkgs.docker}/bin/docker";
          in
          "${pkgs.bash}/bin/bash -c '${pkgs.util-linux}/bin/nsenter -t 1 -m -u -i -n -p -- ${cmd} network create --subnet=10.90.0.0/24 dnf-net 2>/dev/null || true'";
      };
    };

    # OCI Containers configuration
    virtualisation.oci-containers = {
      backend = cfg.backend;
      containers = {
        mysql = {
          image = "docker.io/1995chen/mysql:7-5.0.95";
          environment = {
            TZ = "Asia/Shanghai";
          };
          ports = [
            "${toString cfg.mysqlPort}:4000"
          ];
          volumes = [
            "${cfg.dataDir}/mysql:/var/lib/mysql"
          ];
          extraOptions = [
            "--memory=1g"
            "--network=dnf-net"
            "--hostname=mysql"
            "--ip=10.90.0.10"
            "--env-file=/run/dnf-server/env"
          ];
        };
        dnf-1 = {
          image = "docker.io/1995chen/dnf:centos5-2.1.5";
          hostname = "dnf-1";
          dependsOn = [ "mysql" ];
          environment = {
            TZ = "Asia/Shanghai";
            SERVER_GROUP = toString cfg.serverGroup;
            OPEN_CHANNEL = cfg.openChannels;
            PUBLIC_IP = cfg.publicIP;
            MYSQL_HOST = "10.90.0.10"; # Use fixed IP address
            MYSQL_PORT = "4000"; # MySQL's internal port
            GM_ACCOUNT = cfg.gmAccount;
            GM_LANDER_VERSION = cfg.gmLanderVersion;
          };
          ports = [
            "2000:180/tcp" # supervisor web
            "7600:7600/tcp" # 统一登陆器
            "881:881/tcp" # 统一网关
            "7001:7001/tcp" # df_channel_r
            "7001:7001/udp" # df_channel_r
            "7300:7300/tcp" # df_relay_r
            "7300:7300/udp" # df_relay_r
            "30011:30011/tcp" # df_game_r[ch.11]
            "31011:31011/udp" # df_game_r[ch.11]
            "30052:30052/tcp" # df_game_r[ch.52]
            "31052:31052/udp" # df_game_r[ch.52]
            "2311-2313:2311-2313/udp" # df_stun_r
          ];
          volumes = [
            "${cfg.dataDir}/data:/data"
            "${cfg.dataDir}/log:/home/neople/game/log"
          ];
          extraOptions = [
            "--shm-size=8g"
            "--memory=1g"
            "--memory-swap=-1"
            "--cpus=1.0"
            "--network=dnf-net"
            "--ip=10.90.0.20"
            "--env-file=/run/dnf-server/env"
          ];
        };
      };
    };

    # Ensure dnf-1 starts after mysql container
    systemd.services."${cfg.backend}-dnf-1" = {
      after = [
        "${cfg.backend}-mysql.service"
        "create-dnf-network.service"
        "create-dnf-env.service"
      ];
      requires = [
        "${cfg.backend}-mysql.service"
        "create-dnf-network.service"
        "create-dnf-env.service"
      ];
      serviceConfig = {
        Restart = "always";
        RestartSec = "10s";
      };
    };

    # Ensure mysql container restarts on failure
    systemd.services."${cfg.backend}-mysql" = {
      after = [ "create-dnf-network.service" "create-dnf-env.service" ];
      requires = [ "create-dnf-network.service" "create-dnf-env.service" ];
      serviceConfig = {
        Restart = "always";
        RestartSec = "10s";
      };
    };

    # Open firewall ports
    networking.firewall = {
      allowedTCPPorts = [
        2000 # supervisor web
        cfg.mysqlPort # MySQL
        7600 # 统一登陆器
        881 # 统一网关
        7001 # df_channel_r
        7300 # df_relay_r
        30011 # df_game_r[ch.11]
        30052 # df_game_r[ch.52]
      ];

      allowedUDPPorts = [
        7001 # df_channel_r
        7300 # df_relay_r
        31011 # df_game_r[ch.11]
        31052 # df_game_r[ch.52]
      ];

      allowedUDPPortRanges = [
        {
          from = 2311;
          to = 2313;
        } # df_stun_r
      ];
    };

    # Add management tools
    environment.systemPackages = with pkgs; [
      mariadb.client # MySQL client tools
    ];
  };
}
