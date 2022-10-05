{ config, pkgs, lib, symlinkJoin, domain, ... }:
let
  dbDomain = "freeman.engineer";
  realm = "FREEMAN.ENGINEER";
  dbSuffix = "dc=freeman,dc=engineer";
  defaultUser = "freeman";
  ldapRootUser = "admin";
  kerberosLdapPassword = "a";
in

{
  services =
    {
      openldap =
        {
          enable = true;
          urlList = [ "ldap:///" "ldapi:///" "ldaps:///" ];
          package = (pkgs.openldap.overrideAttrs (old: {
            configureFlags = old.configureFlags ++ [
              "--enable-spasswd"
              "--with-cyrus-sasl"
            ];
            doCheck = false;
          })).override
            {
              cyrus_sasl = pkgs.cyrus_sasl_with_ldap;
            };

          settings =
            {
              attrs = let credsDir = config.security.acme.certs."${domain}".directory; in
                {
                  olcLogLevel = [ "stats" ];
                  olcSaslHost = "freeman.engineer";
                  olcSaslRealm = "FREEMAN.ENGINEER";
                  olcTLSCACertificateFile = credsDir + "/full.pem";
                  olcTLSCertificateFile = credsDir + "/cert.pem";
                  olcTLSCertificateKeyFile = credsDir + "/key.pem";
                  olcAuthzRegexp = [
                    "{0}uid=${ldapRootUser},cn=gssapi,cn=auth cn=${ldapRootUser},${dbSuffix}"
                    "{1}uid=${ldapRootUser}/admin,cn=gssapi,cn=auth cn=${ldapRootUser},${dbSuffix}"
                    "{2}uid=([^,]*),cn=gssapi,cn=auth uid=\$1,ou=accounts,ou=posix,${dbSuffix}"
                  ];
                };
              children = {
                "cn=schema" = {
                  includes = [
                    "${pkgs.openldap}/etc/schema/core.ldif"
                    "${pkgs.openldap}/etc/schema/collective.ldif"
                    "${pkgs.openldap}/etc/schema/corba.ldif"
                    "${pkgs.openldap}/etc/schema/cosine.ldif"
                    "${pkgs.openldap}/etc/schema/duaconf.ldif"
                    "${pkgs.openldap}/etc/schema/dyngroup.ldif"
                    "${pkgs.openldap}/etc/schema/inetorgperson.ldif"
                    "${pkgs.openldap}/etc/schema/java.ldif"
                    "${pkgs.openldap}/etc/schema/nis.ldif"
                    "${pkgs.openldap}/etc/schema/misc.ldif"
                    "${pkgs.openldap}/etc/schema/openldap.ldif"
                    "${pkgs.openldap}/etc/schema/pmi.ldif"
                    ../../../common/kerberos.ldif
                    ../../../common/sudoers.ldif
                  ];
                };
                "cn=module{0}" = {
                  attrs = {
                    objectClass = [ "olcModuleList" ];
                    cn = "module{0}";
                    olcModuleLoad = "{0}back_monitor";
                  };
                };
                "olcDatabase={1}mdb" = {
                  attrs = {
                    objectClass = [ "olcDatabaseConfig" "olcMdbConfig" ];
                    olcDbIndex = "krbPrincipalName eq,pres,sub";
                    olcDatabase = "{1}mdb";
                    olcDbDirectory = "/var/lib/openldap/ldap";
                    olcSuffix = "${dbSuffix}";
                    olcRootDN = "cn=${ldapRootUser},${dbSuffix}";
                    olcRootPW = "{SASL}${ldapRootUser}@${realm}";
                    olcAccess = [
                      "{0}to attrs=userPassword by self write by dn.base=\"cn=${ldapRootUser},${dbSuffix}\" write by anonymous auth by * none"
                      "{1}to attrs=krbPrincipalKey by anonymous auth by dn.exact=\"uid=kdc,ou=services,${dbSuffix}\" read by dn.exact=\"uid=kadmin,ou=services,${dbSuffix}\" write by self write by * none"
                      "{2}to dn.subtree=\"ou=services,${dbSuffix}\"
                by dn.exact=\"uid=kdc,ou=services,${dbSuffix}\" read
                by dn.exact=\"uid=kadmin,ou=services,${dbSuffix}\" write
                by * none"
                      "{3}to * by dn.base=\"cn=${ldapRootUser},${dbSuffix}\" write by self write by * read"
                    ];
                  };
                };
                "olcDatabase={2}monitor" = {
                  attrs = {
                    objectClass = [ "olcDatabaseConfig" "olcMonitorConfig" ];
                    olcAccess = [
                      "{0}to * by dn.base=\"gidNumber=0+uidNumber=0,cn=peercred,cn=external,cn=auth\" read by dn.base=\"cn=${ldapRootUser},${dbSuffix}\" read by * none"
                    ];
                  };
                };
              };
            };
          declarativeContents = {
            ${dbSuffix} = ''
              dn: ${dbSuffix}
              objectClass: top
              objectClass: dcObject
              objectClass: organization
              o: ${dbDomain}

              dn: ou=services,${dbSuffix}
              objectClass: top
              objectClass: organizationalUnit

              dn: uid=kdc, ou=services,${dbSuffix}
              objectClass: account
              objectClass: simpleSecurityObject
              userPassword: ${kerberosLdapPassword}
              description: Account used for the Kerberos KDC

              dn: uid=kadmin, ou=services,${dbSuffix}
              objectClass: account
              objectClass: simpleSecurityObject
              userPassword: ${kerberosLdapPassword}
              description: Account used for the Kerberos Admin server

              dn: ou=developers,${dbSuffix}
              objectClass: top
              objectClass: organizationalUnit

              dn: cn=developers,ou=developers,${dbSuffix}
              objectClass: posixGroup
              cn: developers
              gidNumber: 1234

              dn: uid=${defaultUser},ou=developers,${dbSuffix}
              objectClass: person
              objectClass: posixAccount
              objectClass: inetOrgPerson
              homeDirectory: /home/${defaultUser}
              userpassword: {SASL}${defaultUser}@${realm}
              uidNumber: 1234
              gidNumber: 1234
              cn: ${defaultUser}
              sn: ${defaultUser}
              mail: fdsa@google.com
              jpegPhoto: www.baidu.com

              dn: uid=user3,ou=developers,${dbSuffix}
              objectClass: person
              objectClass: posixAccount
              objectClass: inetOrgPerson
              homeDirectory: /home/user3
              userpassword: {SASL}user3@${realm}
              uidNumber: 1235
              gidNumber: 1234
              cn: user3
              sn: user3
              mail: fdsa@google.com
              jpegPhoto: www.baidu.com

              dn: uid=user4,ou=developers,${dbSuffix}
              objectClass: person
              objectClass: posixAccount
              objectClass: inetOrgPerson
              homeDirectory: /home/user4
              userpassword: {SASL}user4@${realm}
              uidNumber: 1236
              gidNumber: 1234
              cn: user4
              sn: user4
              mail: fdsa@google.com
              jpegPhoto: www.baidu.com

              dn: ou=SUDOers,${dbSuffix}
              objectClass: top
              objectClass: organizationalUnit

              dn: cn=defaults,ou=SUDOers,${dbSuffix}
              objectClass: top
              objectClass: sudoRole
              cn: defaults
              description: Default sudoOptions go here
              sudoOption: env_keep+=SSH_AUTH_SOCK
            '';
          };
          mutableConfig = true;
        };
    };
}

