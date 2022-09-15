{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.services.bttc;
  seeds = pkgs.writeText "delivery-seeds.txt" cfg.seeds;
in
{
  imports = [ ];
  options.services.bttc = {
    enable = mkEnableOption "Enables the go bttc service";
    dataDir = mkOption {
      description = lib.mdDoc "Data directory.";
      default = "/var/lib/bttc";
      type = types.path;
    };
    seeds = mkOption {
      description = lib.mdDoc "Seeds.";
      default = "";
      type = types.str;
    };
  };

  config = mkIf cfg.enable {
    systemd.services = {
      bttc = {
        description = "Bttc Service Daemon";
        wantedBy = [ "multi-user.target" ];
        after = [ "networking.target" ];
        startLimitIntervalSec = 500;
        startLimitBurst = 5;
        serviceConfig = {
          User = "bttc";
          Restart = "on-failure";
          ExecStart = ''
            ${pkgs.bttc}/bin/bttc --datadir ${cfg.dataDir}
          '';
          EnvironmentFile = "/etc/Bttc/metadata";
          ExecStartPre = "/bin/chmod +x $NODE_DIR/bttc/start.sh";
          ExecStart = "/bin/bash $NODE_DIR/bttc/start.sh $VALIDATOR_ADDRESS";
          RestartSec = "5s";
          DynamicUser = "yes";
          WorkingDirectory = cfg.dataDir;
          RuntimeDirectory = "runtime";
          RuntimeDirectoryMode = "0755";
          StateDirectory = "bttc";
          StateDirectoryMode = "0700";
          CacheDirectory = "cache";
          CacheDirectoryMode = "0750";
          SystemCallArchitectures = "native";
          Type = "simple";
          KillSignal = "SIGINT";
          TimeoutStopSec = 120;
        };
      };
      deliveryd = {
        description = "Bttc Service Daemon";
        wantedBy = [ "multi-user.target" ];
        after = [ "networking.target" ];
        startLimitIntervalSec = 500;
        startLimitBurst = 5;
        serviceConfig =
          {
            User = "bttc";
            Restart = "on-failure";
            ExecStart = ''
              ${pkgs.delivery}/bin/deliveryd start
            '';
            EnvironmentFile = "/etc/Bttc/metadata";
            ExecStartPre = "/bin/chmod +x $NODE_DIR/bttc/start.sh";
            ExecStart = "/bin/bash $NODE_DIR/bttc/start.sh $VALIDATOR_ADDRESS";
            RestartSec = "5s";
            DynamicUser = "yes";
            WorkingDirectory = cfg.dataDir;
            RuntimeDirectory = "runtime";
            RuntimeDirectoryMode = "0755";
            StateDirectory = "bttc";
            StateDirectoryMode = "0700";
            CacheDirectory = "cache";
            CacheDirectoryMode = "0750";
            SystemCallArchitectures = "native";
            Type = "simple";
            KillSignal = "SIGINT";
            TimeoutStopSec = 120;
          };

      };
      deliveryd-rest-server = {
        description = "Bttc Service Daemon";
        wantedBy = [ "multi-user.target" ];
        after = [ "networking.target" ];
        startLimitIntervalSec = 500;
        startLimitBurst = 5;
        serviceConfig =
          {
            User = "bttc";
            Restart = "on-failure";
            ExecStart = ''
              ${pkgs.delivery}/bin/deliveryd rest-server
            '';
            EnvironmentFile = "/etc/Bttc/metadata";
            ExecStartPre = "/bin/chmod +x $NODE_DIR/bttc/start.sh";
            ExecStart = "/bin/bash $NODE_DIR/bttc/start.sh $VALIDATOR_ADDRESS";
            RestartSec = "5s";
            DynamicUser = "yes";
            WorkingDirectory = cfg.dataDir;
            RuntimeDirectory = "runtime";
            RuntimeDirectoryMode = "0755";
            StateDirectory = "bttc";
            StateDirectoryMode = "0700";
            CacheDirectory = "cache";
            CacheDirectoryMode = "0750";
            SystemCallArchitectures = "native";
            Type = "simple";
            KillSignal = "SIGINT";
            TimeoutStopSec = 120;
          };

      };
      deliveryd-birdge = {
        description = "Bttc Service Daemon";
        wantedBy = [ "multi-user.target" ];
        after = [ "networking.target" ];
        startLimitIntervalSec = 500;
        startLimitBurst = 5;
        serviceConfig =
          let pkg = pkgs.bttc;
          in
          {
            User = "bttc";
            Restart = "on-failure";
            ExecStart = ''
              ${pkgs.delivery}/bin/bridge start --all
            '';
            EnvironmentFile = "/etc/Bttc/metadata";
            ExecStartPre = "/bin/chmod +x $NODE_DIR/bttc/start.sh";
            ExecStart = "/bin/bash $NODE_DIR/bttc/start.sh $VALIDATOR_ADDRESS";
            RestartSec = "5s";
            DynamicUser = "yes";
            WorkingDirectory = cfg.dataDir;
            RuntimeDirectory = "runtime";
            RuntimeDirectoryMode = "0755";
            StateDirectory = "bttc";
            StateDirectoryMode = "0700";
            CacheDirectory = "cache";
            CacheDirectoryMode = "0750";
            SystemCallArchitectures = "native";
            Type = "simple";
            KillSignal = "SIGINT";
            TimeoutStopSec = 120;
          };
      };
    };
    users.users.bttc = {
      description = "bttc user";
      isSystemUser = true;
      home = cfg.dataDir;
      createHome = true;
      group = "bttc";
    };
    users.groups.bttc = { };
  };
}
