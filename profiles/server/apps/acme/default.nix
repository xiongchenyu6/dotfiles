{ config, ... }:
{
  sops.secrets."acme/cloudflare" = {
    mode = "770";
    owner = "acme";
    group = "acme";
  };

  security = {
    pam.services.nginx.setEnvironment = false;

    acme = {
      acceptTerms = true;
      defaults = {
        email = "xiongchenyu6@gmail.com";
        # postRun = ''
        #   ${pkgs.systemd}/bin/systemctl restart openldap
        # '';
      };
      certs = {
        "${config.networking.domain}" = {
          dnsProvider = "cloudflare";
          domain = "*.${config.networking.domain}";
          #          extraDomainNames = [ "*.inner.${config.networking.domain}" ];
          credentialsFile = config.sops.secrets."acme/cloudflare".path;
          # We don't need to wait for propagation since this is a local DNS server
          dnsPropagationCheck = false;
          # reloadServices =
          #   [ "openldap.service" "postfix.service" "dovecot2.service" ];
          group = "nginx";
        };
      };
    };
  };
}
