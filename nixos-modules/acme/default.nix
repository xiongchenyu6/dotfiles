{ config, lib, ... }:
{
  sops.secrets."acme/cloudflare" = {
    mode = "770";
    owner = "acme";
    group = "acme";
  };

  # sops.secrets."acme/volcengine" = {
  #   mode = "770";
  #   owner = "acme";
  #   group = "acme";
  # };

  security = {
    pam.services.nginx.setEnvironment = false;

    acme = {
      acceptTerms = true;
      defaults = {
        email = "xiongchenyu6@gmail.com";
        dnsProvider = "cloudflare";
        dnsResolver = "1.1.1.1:53";
        environmentFile = config.sops.secrets."acme/cloudflare".path;
        #group = "nginx";
        # postRun = ''
        #   ${pkgs.systemd}/bin/systemctl restart openldap
        # '';
      };
      # Only sign a wildcard for the host's own domain when it actually has one.
      # Hosts without a domain previously fell back to the corp default and signed
      # a cert nothing on them ever served.
      # panda.qzz.io is listed last so its richer definition wins when a host's
      # own domain happens to be panda.qzz.io.
      certs =
        lib.optionalAttrs (config.networking.domain != null) {
          ${config.networking.domain} = {
            domain = config.networking.domain;
            extraDomainNames = [ "*.${config.networking.domain}" ];
          };
        }
        // {
          "panda.qzz.io" = {
            domain = "panda.qzz.io";
            extraDomainNames = [ "*.panda.qzz.io" ];
            group = "nginx";
            reloadServices = [ "nginx.service" ];
          };
        };
    };
  };
}
