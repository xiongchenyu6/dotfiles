{ config, pkgs, lib, profiles, ... }:
let
  realm = "FREEMAN.ENGINEER";
  dbSuffix = "dc=freeman,dc=engineer";
  ldapRootUser = "admin";
  secrets-files-path = ../../../secrets;
  kdcPasswordFile = secrets-files-path + "/kdc.password";
  kadminPasswordFile = secrets-files-path + "/kadmin.password";
in {
  sops.secrets."openldap/credentials" = {
    mode = "770";
    owner = "openldap-exporter";
    group = "openldap-exporter";
  };

  users = {
    users = {
      openldap-exporter = {
        group = "openldap-exporter";
        isSystemUser = true;
      };
    };
    groups.openldap-exporter = { };
  };

  services = {
    prometheus.exporters = {
      openldap = {
        enable = true;
        port = 9007;
        ldapCredentialFile = config.sops.secrets."openldap/credentials".path;
      };
    };
    openldap = {
      enable = true;
      urlList = [ "ldap:///" "ldapi:///" "ldaps:///" ];
      package = pkgs.openldap_with_cyrus_sasl;
      settings = {
        attrs = let
          credsDir =
            config.security.acme.certs."${config.networking.fqdn}".directory;
        in {
          olcLogLevel = [ "stats" ];
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
              "${pkgs.openldap}/etc/schema/misc.ldif"
              "${pkgs.openldap}/etc/schema/openldap.ldif"
              "${pkgs.openldap}/etc/schema/pmi.ldif"
              "${pkgs.ldap-extra-schemas}/kerberos.ldif"
              "${pkgs.ldap-extra-schemas}/sudoers.ldif"
              "${pkgs.ldap-extra-schemas}/rfc2307bis.ldif"
              "${pkgs.ldap-extra-schemas}/openssh-lpk.ldif"
            ];
          };
          "cn=module{0}" = {
            attrs = {
              objectClass = [ "olcModuleList" ];
              cn = "module{0}";
              olcModuleLoad = [ "{0}dynlist" "{1}back_monitor" "{2}memberof" ];
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
                  objectClass = [ "olcOverlayConfig" "olcDynamicList" ];
                  olcDynListAttrSet = "groupOfURLs memberURL member+dgMemberOf";
                  olcOverlay = "dynlist";
                };
              };
              "olcOverlay={1}memberof" = {
                attrs = {
                  objectClass = [ "olcOverlayConfig" "olcMemberOf" ];
                  olcMemberOfRefint = "TRUE";
                  olcOverlay = "memberof";
                };
              };
            };
          };

          "olcDatabase={2}monitor" = {
            attrs = {
              objectClass = [ "olcDatabaseConfig" "olcMonitorConfig" ];
              olcAccess = [
                ''
                  {0}to * by dn.base="gidNumber=0+uidNumber=0,cn=peercred,cn=external,cn=auth" read by dn.base="cn=${ldapRootUser},${dbSuffix}" read by * none''
              ];
            };
          };
        };
      };
      declarativeContents = with builtins;
        with lib;
        let
          cd = "ou=developers,${dbSuffix}";
          init-uid = 1233;
          init-gid = 8888;

          new-user = gn: sn:
            let cn = "${gn}.${sn}";
            in uid: gid: tel: pk: ''
              dn: uid=${cn},${cd}
              objectClass: person
              objectClass: posixAccount
              objectClass: organizationalPerson
              objectClass: shadowAccount
              objectClass: inetOrgPerson
              objectClass: ldapPublicKey
              homeDirectory: /home/${cn}
              userpassword: {SASL}${cn}@${realm}
              uidNumber: ${toString uid}
              gidNumber: ${toString gid}
              cn: ${cn}
              sn: ${sn}
              givenName: ${gn}
              mail: ${cn}@mail.trontech.link
              jpegPhoto: www.baidu.com
              loginShell: /run/current-system/sw/bin/zsh
              telephoneNumber: ${toString tel}
              sshPublicKey: ${pk}
            '';

          new-group = group: users: gid: ''
            dn: cn=${group},${cd}
            objectClass: groupOfNames
            objectClass: posixGroup
            objectClass: top
            cn: ${group}
            gidNumber: ${toString gid}
            member:
            ${concatMapStringsSep "\n" (user: "member: uid=${user},${cd}")
            users}
          '';

          group-contents = concatImapStringsSep "\n" (pos: x:
            new-group x.name (if (x ? "users") then x.users else [ ])
            (if (x ? "id") then x.id else (pos + init-gid)))
            profiles.share.group-dict;

          user-contents = concatImapStringsSep "\n"
            (pos: x: new-user x.gn x.sn (pos + init-uid) x.gid x.tel x.pk)
            profiles.share.users;
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

            dn: ${cd}
            ou: developers
            objectClass: top
            objectClass: organizationalUnit
            description: Parent object of all UNIX accounts

            ${builtins.trace group-contents group-contents}

            ${builtins.trace user-contents user-contents}

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

            dn: cn=podmans,${cd}
            objectClass: posixGroup
            objectClass: groupOfURLs
            cn: podmans
            gidNumber: 1250
            memberURL: ldap:///${cd}??sub?(objectClass=person)

            dn: cn=grafana,${cd}
            objectClass: groupOfURLs
            cn: grafana
            memberURL: ldap:///${cd}??sub?(objectClass=person)
          '';
        };
      mutableConfig = false;
    };
  };
}
