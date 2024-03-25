# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ config, ... }: {
  services = {
    gotify = {
      enable = true;
      port = 8084;
    };
    nginx = {
      virtualHosts = {
        gotify = {
          serverName = "gotify.inner.${config.networking.domain}";
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "${config.networking.domain}";
          kTLS = true;
          locations."/" = {
            proxyPass =
              "http://localhost:${toString config.services.gotify.port}";
          };
        };
      };
    };
  };
}
