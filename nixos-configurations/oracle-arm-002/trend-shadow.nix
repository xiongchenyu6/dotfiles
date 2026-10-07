{ pkgs, ... }:
let
  python = pkgs.python313.withPackages (ps: [ ps.pandas ps.requests ]);
  source = ./trend-shadow;
in {
  systemd.services.quant-trend-shadow = {
    description = "Frozen prospective trend OHLC evaluation (public data only)";
    serviceConfig = {
      Type = "oneshot";
      DynamicUser = true;
      StateDirectory = "quant-trend-shadow";
      StateDirectoryMode = "0700";
      UMask = "0077";
      ExecStart = "${python}/bin/python ${source}/scripts/trend_shadow.py --home /var/lib/quant-trend-shadow --plan ${source}/research/trend-shadow-plan.json";
      RuntimeMaxSec = 900;
      NoNewPrivileges = true;
      PrivateTmp = true;
      ProtectHome = true;
      ProtectSystem = "strict";
    };
  };
  systemd.timers.quant-trend-shadow = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "2m";
      OnCalendar = "*-*-* *:05:00 UTC";
      Persistent = true;
      Unit = "quant-trend-shadow.service";
    };
  };
}
