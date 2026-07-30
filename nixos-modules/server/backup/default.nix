{ config, ... }:
{

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
        paths = [ "/home" ];
        initialize = true;
        passwordFile = config.sops.secrets."restic/pass".path;
        environmentFile = config.sops.secrets."restic/s3".path;

        # Keep the archive bounded — without a prune policy the repo grows
        # forever and the free tier fills up silently.
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

}
