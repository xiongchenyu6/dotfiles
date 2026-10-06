{ inputs, pkgs, ... }:
let
  python = pkgs.python313.withPackages (ps: [
    inputs.xiongchenyu6.packages.${pkgs.stdenv.hostPlatform.system}.ccxt
    ps.certifi
    ps.pysocks
  ]);
  source = pkgs.runCommand "owner-starslab-runner-source" { } ''
    mkdir -p $out/starslab_runner
    cp ${./starslab-runner}/* $out/starslab_runner/
  '';
in {
  users.groups.starslab-runner = { };
  users.users.starslab-runner = {
    isSystemUser = true;
    group = "starslab-runner";
    home = "/var/lib/starslab-runner";
  };
  systemd.services.owner-starslab-runner = {
    description = "Personal owner-operated HTX spot runner";
    wantedBy = [ "multi-user.target" ];
    wants = [ "network-online.target" ];
    after = [ "network-online.target" ];
    unitConfig.ConditionPathExists = "/var/lib/starslab-runner/config.json";
    environment.PYTHONPATH = "${source}";
    serviceConfig = {
      User = "starslab-runner";
      Group = "starslab-runner";
      StateDirectory = "starslab-runner";
      StateDirectoryMode = "0700";
      WorkingDirectory = "/var/lib/starslab-runner";
      ExecStart = "${python}/bin/python -m starslab_runner --home /var/lib/starslab-runner run";
      Restart = "on-failure";
      RestartSec = 15;
      UMask = "0077";
      NoNewPrivileges = true;
      PrivateTmp = true;
      ProtectHome = true;
      ProtectSystem = "strict";
      ReadWritePaths = [ "/var/lib/starslab-runner" ];
    };
  };
  my.backup.paths = [ "/var/lib/starslab-runner" ];
}
