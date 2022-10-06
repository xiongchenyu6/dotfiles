{ config, pkgs, options, lib, ... }:

{
  krb5 = {
    enable = true;
    realms = {
      "FREEMAN.ENGINEER" = {
        admin_server = "freeman.engineer";
        kdc = [ "freeman.engineer" ];
        default_domain = "freeman.engineer";
        kpasswd_server = "freeman.engineer";
        database_module = "openldap_ldapconf";

      };
    };
    libdefaults = {
      default_realm = "FREEMAN.ENGINEER";
      dns_lookup_realm = true;
      dns_lookup_kdc = true;
      dns_fallback = true;
      rdns = false;
      ignore_acceptor_hostname = true;
    };
    domain_realm = {
      "freeman.engineer" = "FREEMAN.ENGINEER";
      ".freeman.engineer" = "FREEMAN.ENGINEER";
    };
  };
}
