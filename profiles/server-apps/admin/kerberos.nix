{ config, pkgs, ... }:
let
  realm = "FREEMAN.ENGINEER";
  dbSuffix = "dc=freeman,dc=engineer";
in {
  sops.secrets."openldap/passwordFile" = { mode = "770"; };
  krb5 = {
    enable = true;
    appdefaults = {
      pam = {
        debug = true;
        ticket_lifetime = 36000;
        renew_lifetime = 36000;
        max_timeout = 30;
        timeout_shift = 2;
        initial_timeout = 1;
      };
    };
    plugins = { ldap_util = { }; };
    extraConfig = ''
      [dbmodules]
          openldap_ldapconf = {
              db_module_dir = ${pkgs.krb5}/lib/krb5/plugins/kdb/
              db_library = kldap
              ldap_servers = ldaps://${config.networking.fqdn}
              ldap_kerberos_container_dn = cn=krbContainer,ou=services,${dbSuffix}
              ldap_kdc_dn = uid=kdc,ou=services,${dbSuffix}
              ldap_kadmind_dn = uid=kadmin,ou=services,${dbSuffix}
              ldap_service_password_file = ${
                config.sops.secrets."openldap/passwordFile".path
              }
              ldap_conns_per_server = 5
       }
    '';
  };

  services = {
    kerberos_server = {
      enable = true;
      realms = {
        "${realm}" = {
          acl = [
            {
              access = "all";
              principal = "*/admin";
            }
            {
              access = "all";
              principal = "admin";
            }
          ];
        };
      };
    };
  };
}
