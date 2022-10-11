{ config, pkgs, lib, ... }:
let
  script = import ./update-roa.nix { inherit pkgs; };
in
{
  systemd = {
    timers = {
      dn42-roa = {
        description = "Trigger a ROA table update";

        timerConfig = {
          OnBootSec = "5m";
          OnUnitInactiveSec = "1h";
          Unit = "dn42-roa.service";
        };

        wantedBy = [ "timers.target" ];
        before = [ "bird2.service" ];
      };
    };
    services = {
      dn42-roa = {
        after = [ "network.target" ];
        description = "DN42 ROA Updated";
        unitConfig = { Type = "one-shot"; };
        serviceConfig = { ExecStart = "${script}/bin/update-roa"; };
      };
    };
  };
}
