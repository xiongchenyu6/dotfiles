{
  config,
  lib,
  pkgs,
  ...
}:
{
  services = {
    jellyfin = {
      enable = true;
      openFirewall = true;
    };

    radarr = {
      enable = true;
      openFirewall = true;
    };

    sonarr = {
      enable = true;
      openFirewall = true;
    };

    aria2 = {
      enable = true;
      openPorts = true;
      downloadDir = "/srv/media/downloads";
      rpcSecretFile = config.sops.secrets."aria2/rpc-secret".path;
      settings = {
        enable-rpc = true;
        rpc-listen-all = true;
        rpc-allow-origin-all = true;
        max-concurrent-downloads = 5;
        max-connection-per-server = 16;
        split = 16;
        min-split-size = "1M";
        continue = true;
        auto-file-renaming = false;
      };
    };
  };

  sops.secrets."aria2/rpc-secret" = { };

  # Shared media group for access control
  users.groups.media = { };

  # Add jellyfin, radarr, sonarr, aria2 users to media group
  users.users.jellyfin.extraGroups = [ "media" ];
  users.users.radarr.extraGroups = [ "media" ];
  users.users.sonarr.extraGroups = [ "media" ];

  systemd.services.aria2.serviceConfig.Group = lib.mkForce "media";

  # Media directory structure
  systemd.tmpfiles.rules = [
    "d /srv/media 0775 root media"
    "d /srv/media/movies 0775 root media"
    "d /srv/media/tv 0775 root media"
    "d /srv/media/downloads 0775 root media"
  ];
}
