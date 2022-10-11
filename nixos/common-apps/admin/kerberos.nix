{ config, pkgs, options, lib, ... }:
let
  realm = "FREEMAN.ENGINEER";
in

{
  krb5 = {
    enable = true;
    realms = {
      "${realm}" = {
        admin_server = config.networking.fqdn;
        kdc = [ config.networking.fqdn ];
        default_domain = config.networking.fqdn;
        kpasswd_server = config.networking.fqdn;
        database_module = "openldap_ldapconf";

      };
    };
    libdefaults = {
      default_realm = realm;
      dns_lookup_realm = true;
      dns_lookup_kdc = true;
      dns_fallback = true;
      rdns = false;
      ignore_acceptor_hostname = true;
    };
    domain_realm = {
      "${config.networking.domain}" = realm;
      ".${config.networking.domain}" = realm;
    };
  };
}
