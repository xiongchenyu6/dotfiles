{ pkgs, ... }:
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
        before = [ "bird.service" ];
      };
    };
    services = {
      # bird includes the ROA files, so the first fetch must land before it starts.
      bird = {
        wants = [ "dn42-roa.service" ];
        after = [ "dn42-roa.service" ];
      };
      dn42-roa = {
        wants = [ "network-online.target" ];
        after = [ "network-online.target" ];
        description = "DN42 ROA Updated";
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "${script}/bin/update-roa";
        };
      };
    };
  };
}
