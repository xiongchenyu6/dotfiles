{ config, lib, pkgs, ... }: {

  sops.secrets."restic/pass" = { };
  sops.secrets."restic/s3" = { };

  services.restic = {
    backups = {
      app = {
        repository =
          "s3:https://https://s3.tebi.io/freeman-bachup/${config.networking.hostName}";
        paths = [ "/home" ];
        initialize = true;
        passwordFile = config.sops.secrets."restic/pass".path;
        environmentFile = config.sops.secrets."restic/s3".path;
      };
    };
    server = {
      enable = false;
      listenAddress = "localhost:18001";
    };
  };

}
