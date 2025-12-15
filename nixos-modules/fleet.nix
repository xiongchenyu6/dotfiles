{
  config,
  lib,
  pkgs,
  ...
}:
with lib;
let
  cfg = config.services.fleet-osquery;
in
{
  options.services.fleet-osquery = {
    enable = mkEnableOption "Fleet osquery management service";
    
    serverUrl = mkOption {
      type = types.str;
      description = "Fleet server URL";
    };

    enrollSecret = mkOption {
      type = types.str;
      description = "Fleet enrollment secret";
    };

    configFile = mkOption {
      type = types.path;
      description = "Osquery configuration file";
    };
  };

  config = mkIf cfg.enable {
    # Install osquery
    environment.systemPackages = with pkgs; [ osquery ];

    # Create osquery configuration
    environment.etc."osquery/osquery.conf".text = builtins.readFile cfg.configFile;

    # SystemD service for osquery
    systemd.services.osquery = {
      description = "osquery daemon";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];

      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.osquery}/bin/osqueryd --config_path /etc/osquery/osquery.conf --enroll_secret_env=OSQUERY_ENROLL_SECRET";
        Restart = "always";
        RestartSec = 5;
        Environment = "OSQUERY_ENROLL_SECRET=${cfg.enrollSecret}";
      };
    };
  };
}