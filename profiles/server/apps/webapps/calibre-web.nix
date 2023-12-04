# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ config, ... }: {
  services = {
    calibre-web = {
      enable = true;
      listen.port = 8083;
      options.enableBookUploading = true;
      openFirewall = true;
    };
    nginx = {
      virtualHosts = {
        calibre-web = {
          serverName = "calibre-web.inner.${config.networking.domain}";
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "inner.${config.networking.domain}";
          kTLS = true;
          locations."/" = {
            proxyPass = "http://localhost:${
                toString config.services.calibre-web.listen.port
              }";
          };
        };
      };
    };
  };
}
