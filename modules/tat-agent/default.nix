{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.services.tat-agent;
in
{
  options.services.tat-agent = {
    enable = mkEnableOption "Enables unit-status-telegram service";
  };
  config = {
    systemd.services = mkIf cfg.enable {
      tat-agent = {
        description = "Bttc Service Daemon";
        path = [ pkgs.curl ];
        wantedBy = [ "multi-user.target" ];
        after = [ "networking.target" ];
        script = ''
          ${pkgs.tat}/bin/tat_agent
        '';
        serviceConfig = {
          User = "root";
          Type = "forking";
          Restart = "always";
          RestartSec = "1s";
          WorkingDirectory = "/usr/local/qcloud";
          PIDFile = "/var/run/tat_agent.pid";
          ExecStartPost = ''${pkgs.coreutils}/bin/sleep 0.2'';
          KillMode = "process";
        };
      };
    };
  };
}
