{ config, ... }: {

  # Restic secrets - keeping the original structure as it's all secrets
  sops.secrets."restic/pass" = { };
  sops.secrets."restic/s3" = { };  # This contains the AWS credentials as environment variables

  services.restic = {
    backups = {
      app = {
        # Scheme appears once: restic wants `s3:<endpoint-url>`, so a doubled
        # https:// makes minio parse an empty bucket name ("Bucket name cannot
        # be empty") and every run dies in pre-start.
        repository = "s3:https://s3.tebi.io/freeman-bachup/${config.networking.hostName}";
        paths = [ "/home" ];
        initialize = true;
        passwordFile = config.sops.secrets."restic/pass".path;
        environmentFile = config.sops.secrets."restic/s3".path;  # Use the s3 secret directly
      };
    };
    server = {
      enable = false;
      listenAddress = "localhost:18001";
    };
  };

}
