{ config, pkgs, inputs, ... }:

{
  imports = [
    # ... other imports ...
    ../../nixos-modules/rust-web-server.nix  # Use the new module with templates
  ];

  # Configure rust-web-server using the new module options
  services.rust-web-server = {
    enable = true;
    port = 3000;
    host = "localhost";
    databaseUrl = "postgresql://rust_web_server:@localhost/rust_web_server";
    logLevel = "info";
  };

  # The module now handles:
  # - Creating the sops secrets for passwords/keys only
  # - Using sops.templates to generate the config file
  # - Properly separating configuration from secrets

  # Nginx configuration remains the same
  services.nginx = {
    virtualHosts = {
      "rust-server.${config.networking.domain}" = {
        forceSSL = true;
        acmeRoot = null;
        useACMEHost = "${config.networking.domain}";
        kTLS = true;
        locations = {
          "/" = {
            proxyPass = "http://localhost:${toString config.services.rust-web-server.port}";
          };
        };
      };
    };
  };
}