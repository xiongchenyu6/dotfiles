{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.my.backup;
  dumpDir = "/var/backup/dumps";

  pgDump = ''
    ${pkgs.coreutils}/bin/install -d -m 0700 -o postgres -g postgres ${dumpDir}/postgres
    ${pkgs.util-linux}/bin/runuser -u postgres -- \
      ${config.services.postgresql.package}/bin/pg_dumpall --clean --if-exists \
      > ${dumpDir}/postgres/all.sql.tmp
    ${pkgs.coreutils}/bin/mv ${dumpDir}/postgres/all.sql.tmp ${dumpDir}/postgres/all.sql
  '';

  mysqlDump = ''
    ${pkgs.coreutils}/bin/install -d -m 0700 ${dumpDir}/mysql
    ${config.services.mysql.package}/bin/mysqldump --all-databases --single-transaction \
      --quick --routines --triggers --events \
      > ${dumpDir}/mysql/all.sql.tmp
    ${pkgs.coreutils}/bin/mv ${dumpDir}/mysql/all.sql.tmp ${dumpDir}/mysql/all.sql
  '';
in
{
  options.my.backup = {
    paths = lib.mkOption {
      type = with lib.types; listOf str;
      default = [ ];
      example = [ "/var/lib/nautilus-accumulator" ];
      description = ''
        Extra paths to back up on this host, on top of the SSH host keys and any
        database dumps. Keep the list to state that cannot be rebuilt from git
        or nix — caches, container images and checked-out repos do not belong
        here.
      '';
    };

    postgres = lib.mkOption {
      type = lib.types.bool;
      default = config.services.postgresql.enable;
      description = "Dump all Postgres databases before each run.";
    };

    mysql = lib.mkOption {
      type = lib.types.bool;
      default = config.services.mysql.enable;
      description = "Dump all MySQL databases before each run.";
    };
  };

  config = {
    # Restic → iDrive e2 (S3-compatible). Replaced Tebi, which shut its
    # object-storage service down on 2026-03-31.
    #
    # The repository URL carries the scheme exactly once: restic wants
    # `s3:<endpoint-url>/<bucket>/<prefix>`, and a doubled https:// makes the S3
    # client parse an empty bucket name, failing every run in pre-start.
    sops.secrets."restic/pass" = { };
    # AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY as environment assignments.
    sops.secrets."restic/s3" = { };

    services.restic = {
      backups = {
        app = {
          repository = "s3:https://s3.us-west-1.idrivee2.com/starslab-backup/${config.networking.hostName}";

          # Deliberately NOT /home: on these hosts it is checked-out repos,
          # build caches and downloaded market data — all reproducible, and it
          # was 7 GB of the 7.3 GB we used to ship every night. Back up the
          # state that only exists here.
          #
          # SSH host keys come along because sops-nix derives each host's age
          # identity from ssh_host_ed25519_key: without it a restored host
          # cannot decrypt any of its own secrets.
          paths =
            [ "/etc/ssh" ]
            ++ lib.optional (cfg.postgres || cfg.mysql) dumpDir
            ++ cfg.paths;

          # Logical dumps, not a copy of the live data directory — a file-level
          # snapshot of a running database is not crash-consistent. They are
          # written uncompressed so restic can dedupe against yesterday's dump
          # and compress the result itself.
          backupPrepareCommand = lib.mkIf (cfg.postgres || cfg.mysql) (
            lib.concatStrings [
              "set -euo pipefail\n"
              (lib.optionalString cfg.postgres pgDump)
              (lib.optionalString cfg.mysql mysqlDump)
            ]
          );

          initialize = true;
          passwordFile = config.sops.secrets."restic/pass".path;
          environmentFile = config.sops.secrets."restic/s3".path;

          # Keep the archive bounded — without a prune policy the repo grows
          # forever and the storage quota fills up silently.
          pruneOpts = [
            "--keep-daily 7"
            "--keep-weekly 4"
            "--keep-monthly 6"
          ];
        };
      };
      server = {
        enable = false;
        listenAddress = "localhost:18001";
      };
    };
  };
}
