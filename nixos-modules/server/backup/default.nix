{ config, ... }: {

  # Restic secrets - keeping the original structure as it's all secrets
  sops.secrets."restic/pass" = { };
  sops.secrets."restic/s3" = { };  # This contains the AWS credentials as environment variables

  services.restic = {
    backups = {
      app = {
        repository =
          "s3:https://https://s3.tebi.io/freeman-bachup/${config.networking.hostName}";
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
