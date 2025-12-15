{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.rust-web-server;
in
{
  # This module provides the sops configuration for rust-web-server
  # It works alongside the upstream rust-web-server module

  config = lib.mkIf (cfg.enable or false) {
    # ONLY the actual secrets - passwords and secret keys
    # From the decrypted values you showed:
    # - db-password: rustwebserver
    # - oauth-client-secret: vVuC5jLwHe7y6wF8hdNLgtxJGru2PUk1PwGEVPBCV0wx6hFZ
    sops.secrets = {
      "rust-web-server/db-password" = { };
      "rust-web-server/oauth-client-secret" = { };
    };

    # ALL configuration goes in the template, with placeholders for secrets
    sops.templates."rust-web-server-config" = {
      content = ''
        database:
          url: postgresql://rustwebserver:${
            config.sops.placeholder."rust-web-server/db-password"
          }@localhost/rustwebserver
        oauth:
          client_id: iXPxzvDuhuH9lGHfR3OMIMchnHxhz86c
          client_secret: ${config.sops.placeholder."rust-web-server/oauth-client-secret"}
          redirect_url: https://rust-server.autolife.ai/auth/authorized
          config_url: https://autolife.jp.auth0.com/.well-known/openid-configuration
        oidc:
          client_id: iXPxzvDuhuH9lGHfR3OMIMchnHxhz86c
          issuer: https://autolife.jp.auth0.com/
          authorization_endpoint: https://autolife.jp.auth0.com/authorize
          token_endpoint: https://autolife.jp.auth0.com/oauth/token
          userinfo_endpoint: https://autolife.jp.auth0.com/userinfo
          jwks_uri: https://autolife.jp.auth0.com/.well-known/jwks.json
          revocation_url: https://autolife.jp.auth0.com/oauth/revoke
          introspection_endpoint: https://autolife.jp.auth0.com/oauth/introspect
        auth:
          enabled: true
      '';
      mode = "0600";
      # The owner and group will be created by the upstream rust-web-server module
      # For now, use root to avoid dependency issues
      owner = "root";
      group = "root";
    };

    # User and systemd service are defined by the upstream rust-web-server module
    # We only provide the sops configuration here
  };
}
