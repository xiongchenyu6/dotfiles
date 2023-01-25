# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ config, ... }: {
  sops.secrets."zammad/secret-key-base" = {
    owner = "zammad";
    group = "zammad";
  };
  services = {
    zammad = {
      port = 3005;
      enable = true;
      openPorts = true;
      secretKeyBaseFile = config.sops.secrets."zammad/secret-key-base".path;
    };
    nginx = {
      virtualHosts = {
        zammad = {
          serverName = "zammad.inner.${config.networking.domain}";
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "inner.${config.networking.domain}";
          kTLS = true;
          locations."/" = {
            proxyPass =
              "http://localhost:${toString config.services.zammad.port}";
          };
        };
      };
    };
  };
}
