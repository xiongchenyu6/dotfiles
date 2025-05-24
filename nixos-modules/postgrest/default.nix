{
  pkgs,
  config,
  lib,
  ...
}:
{
  sops.secrets."postgrest/pass" = {
  };
  sops.secrets."postgrest/jwt-secret" = {
  };

  services = {
    postgrest = {
      enable = true;
      pgpassFile = config.sops.secrets."postgrest/pass".path;
      jwtSecretFile = config.sops.secrets."postgrest/jwt-secret".path;
      settings = {
        db-uri = {
          host = "localhost";
          port = "5432";
          user = "rustwebserver";
          dbname = "rustWebServer";
        };
        db-anon-role = "rustwebserver";
        server-port = 3333; # use unix socket
        server-unix-socket = null;
        openapi-server-proxy-uri = "https://api.autolife-robotics.me";
        openapi-security-active = true;
        jwt-role-claim-key = ".roles[0]";
        jwt-cache-max-lifetime = 3600;
      };
    };
  };
}
