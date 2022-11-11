{config, ...}: {
  services.grafana = let
    ldapConfigFile = ./ldap.toml;
  in {
    enable = true;
    # Listening address and TCP port
    # Grafana needs to know on which domain and URL it's running:
    settings = {
      server = {
        domain = "grafana.inner.${config.networking.domain}";
        http_port = 3001;
        http_addr = "127.0.0.1";
        serve_from_sub_path = true;
      };
      "auth.ldap" = {
        enabled = true;
        allow_sign_up = true;
        config_file = "${ldapConfigFile}";
      };
    };
  };
}
