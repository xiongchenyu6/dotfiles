{ config, pkgs, lib, ... }:

{
  services.grafana = let ldapConfigFile = ./ldap.toml; in
    {
      enable = true;
      # Listening address and TCP port
      addr = "127.0.0.1";
      port = 3001;
      # Grafana needs to know on which domain and URL it's running:
      domain = "grafana.inner.${config.networking.domain}";
      server.serveFromSubPath = true;
      extraOptions = {
        AUTH_LDAP_ENABLED = "true";
        AUTH_LDAP_ALLOW_SIGN_UP = "true";
        AUTH_LDAP_CONFIG_FILE = "${ldapConfigFile}";
      };
    };
}
