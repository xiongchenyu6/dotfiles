{ config, pkgs, lib, ... }:

{
  services.grafana = let ldapConfigFile = ./ldap.toml;
  in {
    enable = true;
    # Listening address and TCP port
    addr = "127.0.0.1";
    port = 3001;
    # Grafana needs to know on which domain and URL it's running:
    domain = "grafana.inner.${config.networking.domain}";
    server.serveFromSubPath = true;
    settings = {
      auth_ldap = {
        enable = true;
        allow_sign_up = true;
        config_file = "${ldapConfigFile}";
      };
    };
  };
}
