{ config, pkgs, lib, ... }:

{

  sops.secrets."acme/namecom" = {
    mode = "770";
    owner = "acme";
    group = "acme";
  };

  security = {
    pam.services.nginx.setEnvironment = false;

    acme = {
      acceptTerms = true;
      defaults = {
        email = "xiongchenyu6@gmail.cam";
        # postRun = ''
        #   ${pkgs.systemd}/bin/systemctl restart openldap
        # '';
      };
      certs = {
        "${config.networking.fqdn}" = {
          dnsProvider = "namedotcom";
          domain = "*.${config.networking.domain}";
          credentialsFile = config.sops.secrets."acme/namecom".path;
          # We don't need to wait for propagation since this is a local DNS server
          dnsPropagationCheck = false;
          reloadServices =
            [ "openldap.service" "postfix.service" "dovecot2.service" ];
          group = "openldap";
        };
        "inner.${config.networking.domain}" = {
          domain = "*.inner.${config.networking.domain}";
          dnsProvider = "rfc2136";
          credentialsFile = "/var/lib/secrets/certs.secret";
          # We don't need to wait for propagation since this is a local DNS server
          dnsPropagationCheck = false;
          group = "nginx";
        };
      };
    };
  };
}
