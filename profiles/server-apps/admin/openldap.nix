{
  config,
  pkgs,
  lib,
  ...
}: let
  realm = "FREEMAN.ENGINEER";
  dbSuffix = "dc=freeman,dc=engineer";
  defaultUser = "freeman.xiong";
  ldapRootUser = "admin";
  secrets-files-path = ../../../secrets;
  kdcPasswordFile = secrets-files-path + "/kdc.password";
  kadminPasswordFile = secrets-files-path + "/kadmin.password";
in {
  services = {
    openldap = {
      enable = true;
      urlList = ["ldap:///" "ldapi:///" "ldaps:///"];
      package = pkgs.openldap_with_cyrus_sasl;
      settings = {
        attrs = let
          credsDir =
            config.security.acme.certs."${config.networking.fqdn}".directory;
        in {
          olcLogLevel = ["stats"];
          olcSaslHost = config.networking.fqdn;
          olcSaslRealm = realm;
          olcTLSCACertificateFile = credsDir + "/full.pem";
          olcTLSCertificateFile = credsDir + "/cert.pem";
          olcTLSCertificateKeyFile = credsDir + "/key.pem";
          olcAuthzRegexp = [
            "{0}uid=${ldapRootUser},cn=gssapi,cn=auth cn=${ldapRootUser},${dbSuffix}"
            "{1}uid=${ldapRootUser}/admin,cn=gssapi,cn=auth cn=${ldapRootUser},${dbSuffix}"
            "{2}uid=([^,]*),cn=gssapi,cn=auth uid=$1,ou=accounts,ou=posix,${dbSuffix}"
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
              "${pkgs.ldap-extra-schemas}/kerberos.ldif"
              "${pkgs.ldap-extra-schemas}/sudoers.ldif"
            ];
          };
          "cn=module{0}" = {
            attrs = {
              objectClass = ["olcModuleList"];
              cn = "module{0}";
              olcModuleLoad = ["{0}dynlist" "{1}back_monitor"];
            };
          };
          "olcDatabase={1}mdb" = {
            attrs = {
              objectClass = ["olcDatabaseConfig" "olcMdbConfig"];
              olcDbIndex = "krbPrincipalName eq,pres,sub";
              olcDatabase = "{1}mdb";
              olcDbDirectory = "/var/lib/openldap/ldap";
              olcSuffix = "${dbSuffix}";
              olcRootDN = "cn=${ldapRootUser},${dbSuffix}";
              olcRootPW = "{SASL}${ldapRootUser}@${realm}";
              olcAccess = [
                ''
                  {0}to attrs=userPassword by self write by dn.base="cn=${ldapRootUser},${dbSuffix}" write by anonymous auth by * none''
                ''
                  {1}to attrs=krbPrincipalKey by anonymous auth by dn.exact="uid=kdc,ou=services,${dbSuffix}" read by dn.exact="uid=kadmin,ou=services,${dbSuffix}" write by self write by * none''
                ''
                  {2}to dn.subtree="ou=services,${dbSuffix}"
                                  by dn.exact="uid=kdc,ou=services,${dbSuffix}" read
                                  by dn.exact="uid=kadmin,ou=services,${dbSuffix}" write
                                  by * none''

                ''
                  {3}to * by dn.base="cn=${ldapRootUser},${dbSuffix}" write by self write by * read''
              ];
            };
            children = {
              "olcOverlay={0}dynlist" = {
                attrs = {
                  objectClass = ["olcOverlayConfig" "olcDynamicList"];
                  olcDynListAttrSet = "groupOfURLs memberURL member+dgMemberOf";
                  olcOverlay = "dynlist";
                };
              };
            };
          };

          "olcDatabase={2}monitor" = {
            attrs = {
              objectClass = ["olcDatabaseConfig" "olcMonitorConfig"];
              olcAccess = [
                ''
                  {0}to * by dn.base="gidNumber=0+uidNumber=0,cn=peercred,cn=external,cn=auth" read by dn.base="cn=${ldapRootUser},${dbSuffix}" read by * none''
              ];
            };
          };
        };
      };
      declarativeContents = with builtins;
      with lib; let
        new-user = cn: uid: gid: tel: ''
          dn: uid=${cn},ou=developers,${dbSuffix}
          objectClass: person
          objectClass: posixAccount
          objectClass: organizationalPerson
          objectClass: shadowAccount
          objectClass: inetOrgPerson
          homeDirectory: /home/${cn}
          userpassword: {SASL}${cn}@${realm}
          uidNumber: ${toString uid}
          gidNumber: ${toString gid}
          cn: ${cn}
          sn: ${cn}
          givenName: ${cn}
          mail: ${cn}@${config.networking.fqdn}
          jpegPhoto: www.baidu.com
          loginShell: /run/current-system/sw/bin/zsh
          telephoneNumber: ${toString tel}
        '';
        init-uid = 1233;
        names = [
          {
            name = defaultUser;
            gid = 1234;
            tel = 1234;
          }
          {
            name = "user3";
            gid = 1233;
            tel = 1234;
          }
          {
            name = "user5";
            gid = 1235;
            tel = 1234;
          }
        ];
        user-contents = concatImapStringsSep "\n" (pos: x: new-user x.name (pos + init-uid) x.gid x.tel) names;
      in {
        ${dbSuffix} = ''
          dn: ${dbSuffix}
          objectClass: top
          objectClass: dcObject
          objectClass: organization
          o: ${config.networking.domain}

          dn: ou=services,${dbSuffix}
          objectClass: top
          objectClass: organizationalUnit

          dn: uid=kdc, ou=services,${dbSuffix}
          objectClass: account
          objectClass: simpleSecurityObject
          description: Account used for the Kerberos KDC
          userPassword: ${builtins.readFile kdcPasswordFile}

          dn: uid=kadmin, ou=services,${dbSuffix}
          objectClass: account
          objectClass: simpleSecurityObject
          description: Account used for the Kerberos Admin server
          userPassword: ${builtins.readFile kadminPasswordFile}

          dn: ou=developers,${dbSuffix}
          ou: developers
          objectClass: top
          objectClass: organizationalUnit
          description: Parent object of all UNIX accounts

          dn: cn=owner,ou=developers,${dbSuffix}
          objectClass: top
          objectClass: posixGroup
          gidNumber: 1233
          description: Linux group used for the Kerberos Admin server

          dn: cn=developer,ou=developers,${dbSuffix}
          objectClass: top
          objectClass: posixGroup
          cn: developer
          gidNumber: 1235
          description: Linux group used for the Kerberos

          ${user-contents}

          dn: ou=SUDOers,${dbSuffix}
          objectClass: top
          objectClass: organizationalUnit

          dn: cn=defaults,ou=SUDOers,${dbSuffix}
          objectClass: top
          objectClass: sudoRole
          cn: defaults
          description: Default sudoOptions go here
          sudoOption: env_keep+=SSH_AUTH_SOCK

          dn: cn=%owner,ou=SUDOers,${dbSuffix}
          objectClass: Top
          objectClass: sudoRole
          sudoRunAsUser: ALL
          sudoHost: 10.0.8.10/22
          sudoUser: %owner
          sudoCommand: ALL

          dn: cn=%developer,ou=SUDOers,${dbSuffix}
          objectClass: Top
          objectClass: sudoRole
          sudoRunAsUser: ALL
          sudoHost: ma*
          sudoUser: %developer
          sudoCommand: ALL

          dn: cn=grafana,ou=developers,${dbSuffix}
          objectClass: groupOfURLs
          cn: grafana
          memberURL: ldap:///ou=developers,${dbSuffix}??sub?(objectClass=person)
        '';
      };
      mutableConfig = false;
    };
  };
}
