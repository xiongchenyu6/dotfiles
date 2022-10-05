{ config, pkgs, lib, symlinkJoin, domain, ... }:

{
  services.grafana = {
    enable = true;
    # Listening address and TCP port
    addr = "127.0.0.1";
    port = 3001;
    # Grafana needs to know on which domain and URL it's running:
    domain = "grafana.inner.${domain}";
    server.serveFromSubPath = true;
    # extraOptions = {
    #   "auth.ldap" = {
    #     enabled = true;
    #     allow_sign_up = true;
    #     config_file = "/etc/grafana/ldap.toml";
    #   };
    # };
  };
}
