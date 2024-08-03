{ config, lib, ... }:
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
        dnsProvider = "cloudflare";
        dnsResolver = "1.1.1.1:53";
        # dnsPropagationCheck = false;
        credentialsFile = config.sops.secrets."acme/cloudflare".path;
        group = "nginx";
        # postRun = ''
        #   ${pkgs.systemd}/bin/systemctl restart openldap
        # '';
      };
      certs = {
        ${config.networking.domain} = {
          domain = "*.autolife-robotics.tech";
        };
        "${config.networking.hostName}.autolife-robotics.me" = {
          domain = "*.autolife-robotics.me";
        };
      };
    };
  };
}
