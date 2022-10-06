{ config, pkgs, lib, symlinkJoin, domain, ... }:

let
  common-files-path = ../../../common;
  secret-files-paht = common-files-path + "/secrets";
  share = import (common-files-path + /share.nix);
in
{

  age.secrets.acme_credentials = {
    file = secret-files-paht + /acme_credentials.age;
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
        "${domain}" = {
          domain = "${domain}";
          dnsProvider = "namedotcom";
          credentialsFile = config.age.secrets.acme_credentials.path;
          # We don't need to wait for propagation since this is a local DNS server
          dnsPropagationCheck = false;
          reloadServices = [
            "openldap.service"
            "postfix.service"
            "dovecot2.service"
          ];
          group = "openldap";
        };
        "inner.${domain}" = {
          domain = "*.inner.${domain}";
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
