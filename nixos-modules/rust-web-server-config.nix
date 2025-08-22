{ config, lib, pkgs, ... }:

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
      "rust-web-server/db-password" = {};
      "rust-web-server/oauth-client-secret" = {};
    };

    # ALL configuration goes in the template, with placeholders for secrets
    sops.templates."rust-web-server-config" = {
      content = ''
        database:
          url: postgresql://rustwebserver:${config.sops.placeholder."rust-web-server/db-password"}@localhost/rustwebserver
        oauth:
          client_id: vr-control
          client_secret: ${config.sops.placeholder."rust-web-server/oauth-client-secret"}
          redirect_url: https://rust-server.auto-life.tech/auth/authorized
          config_url: https://kanidm.auto-life.tech/oauth2/openid/vr-control/.well-known/openid-configuration
        oidc:
          client_id: robot-management-system
          issuer: https://kanidm.auto-life.tech/oauth2/openid/robot-management-system
          authorization_endpoint: https://kanidm.auto-life.tech/oauth2/openid/robot-management-system
          token_endpoint: https://kanidm.auto-life.tech/oauth2/token
          userinfo_endpoint: https://kanidm.auto-life.tech/oauth2/openid/robot-management-system/userinfo
          jwks_uri: https://kanidm.auto-life.tech/oauth2/openid/robot-management-system/public_key.jwk
          revocation_url: https://kanidm.auto-life.tech/oauth2/token/revoke
          introspection_endpoint: https://kanidm.auto-life.tech/oauth2/token/introspect
        auth:
          enabled: false
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