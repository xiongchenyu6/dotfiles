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
        credentialsFile = config.sops.secrets."acme/cloudflare".path;
        #group = "nginx";
        # postRun = ''
        #   ${pkgs.systemd}/bin/systemctl restart openldap
        # '';
      };
      certs = {
        ${config.networking.domain} = {
          domain = "${config.networking.domain}";
          extraDomainNames = [ "*.${config.networking.domain}" ];
        };
        # "auotlife.com" = {
        #   domain = "auotlife.com";
        #   extraDomainNames = [ "*.auotlife.com" ];
        #   email = "xiongchenyu6@gmail.com";
        #   dnsProvider = "volcengine";
        #   credentialsFile = config.sops.secrets."acme/volcengine".path;
        #   group = "kanidm";
        # };
        ai = {
          domain = "autolife.ai";
          extraDomainNames = [ "*.autolife.ai" ];
        };
      };
    };
  };
}
