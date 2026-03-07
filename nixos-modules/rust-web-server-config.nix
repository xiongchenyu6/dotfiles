{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.services.rust-web-server;
  shares = import ../shares.nix { inherit lib; };
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
          client_id: ${shares.oauth.client_id}
          client_secret: ${config.sops.placeholder."rust-web-server/oauth-client-secret"}
          redirect_url: ${shares.oauth.redirect_url}
          config_url: ${shares.oauth.config_url}
        oidc:
          client_id: ${shares.oauth.client_id}
          issuer: ${shares.oauth.issuer}
          authorization_endpoint: ${shares.oauth.authorization_endpoint}
          token_endpoint: ${shares.oauth.token_endpoint}
          userinfo_endpoint: ${shares.oauth.userinfo_endpoint}
          jwks_uri: ${shares.oauth.jwks_uri}
          revocation_url: ${shares.oauth.revocation_url}
          introspection_endpoint: ${shares.oauth.introspection_endpoint}
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
