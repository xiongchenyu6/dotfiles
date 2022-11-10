{ config, ... }:
let
  dbDomain = "freeman.engineer";
  realm = "FREEMAN.ENGINEER";
  dbSuffix = "dc=freeman,dc=engineer";
  ldapRootUser = "admin";
in {
  services = {
    sssd = {
      enable = true;
      config = ''
        [sssd]
        config_file_version = 2
        services = nss, pam, ssh, autofs, sudo
        domains = ${dbDomain}
        [nss]
        [pam]
        [sudo]
        debug_level = 0x3ff0

        [domain/${dbDomain}]
        autofs_provider = ldap
        ldap_schema = rfc2307bis
        id_provider = ldap
        sudo_provider = ldap
        ldap_uri = ldaps://${config.networking.fqdn}
        ldap_search_base = ${dbSuffix}
        ldap_default_bind_dn = cn=${ldapRootUser},${dbSuffix}
        ldap_sudo_search_base = ou=SUDOers,${dbSuffix}
        ldap_sasl_mech = GSSAPI
        ldap_sasl_authid = host/${config.networking.fqdn}
        ldap_sasl_realm = ${realm}

        chpass_provider = krb5

        use_fully_qualified_names = false

        auth_provider = krb5
        access_provider = simple

        krb5_realm = FREEMAN.ENGINEER
        krb5_server = ${config.networking.fqdn}
        krb5_validate = true
        debug_level = 0x3ff0
      '';
    };
  };
}
