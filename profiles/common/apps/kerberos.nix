{ config, ... }:
let realm = "AUTOLIFE.TECH";
in {
  krb5 = {
    realms = {
      "${realm}" = {
        admin_server = "mail.${config.networking.domain}";
        kdc = [ "mail.${config.networking.domain}" ];
        default_domain = "mail.${config.networking.domain}";
        kpasswd_server = "mail.${config.networking.domain}";
        database_module = "openldap_ldapconf";
      };
    };
    libdefaults = {
      default_realm = realm;
      dns_lookup_realm = false;
      dns_lookup_kdc = false;
      dns_fallback = false;
      rdns = false;
      ignore_acceptor_hostname = true;
    };
    domain_realm = {
      "${config.networking.domain}" = realm;
      ".${config.networking.domain}" = realm;
    };
  };
}
