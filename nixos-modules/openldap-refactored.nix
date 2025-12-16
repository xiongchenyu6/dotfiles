{ config, pkgs, lib, shares, ... }:

let
  realm = "AUTOLIFE.TECH";
  dbSuffix = "dc=auotlife,dc=ai";
  ldapRootUser = "admin";
  cfg = config.services.openldap;
in
{
  # Only actual secrets in sops.secrets
  sops.secrets = {
    "openldap/admin-password" = {
      mode = "0600";
      owner = "openldap";
      group = "openldap";
    };
    "openldap/kdc-password" = {
      mode = "0600";
      owner = "openldap";
      group = "openldap";
    };
    "openldap/kadmin-password" = {
      mode = "0600";
      owner = "openldap";
      group = "openldap";
    };
    "openldap/exporter-password" = {
      mode = "0770";
      owner = "openldap-exporter";
      group = "openldap-exporter";
    };
  };

  # Use template for exporter credentials file
  sops.templates."openldap-exporter-credentials" = {
    content = ''
      # OpenLDAP Exporter Credentials
      ldap_uri=ldap://localhost:389
      ldap_user=cn=${ldapRootUser},${dbSuffix}
      ldap_pass=${config.sops.placeholder."openldap/exporter-password"}
    '';
    mode = "0600";
    owner = "openldap-exporter";
    group = "openldap-exporter";
  };

  # Template for LDAP configuration that includes secrets
  sops.templates."openldap-config" = lib.mkIf cfg.enable {
    content = ''
      # OpenLDAP Configuration Template
      # This file contains the LDAP configuration with injected secrets
      
      # Admin credentials
      olcRootPW: ${config.sops.placeholder."openldap/admin-password"}
      
      # Service account passwords
      kdc_password: ${config.sops.placeholder."openldap/kdc-password"}
      kadmin_password: ${config.sops.placeholder."openldap/kadmin-password"}
    '';
    mode = "0600";
    owner = "openldap";
    group = "openldap";
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
        ldapCredentialFile = config.sops.templates."openldap-exporter-credentials".path;
      };
    };
    
    openldap = {
      enable = true;
      urlList = [ "ldap:///" "ldapi:///" ];
      package = pkgs.openldap_with_cyrus_sasl;
      
      settings = {
        attrs = let
          credsDir = config.security.acme.certs."${config.networking.fqdn}".directory;
        in {
          olcLogLevel = [ "stats" ];
          olcSaslHost = config.networking.fqdn;
          olcSaslRealm = realm;
          olcTLSCACertificateFile = "${credsDir}/full.pem";
          olcTLSCertificateFile = "${credsDir}/cert.pem";
          olcTLSCertificateKeyFile = "${credsDir}/key.pem";
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
              olcSuffix = dbSuffix;
              olcRootDN = "cn=${ldapRootUser},${dbSuffix}";
              olcRootPW = "{SASL}${ldapRootUser}@${realm}";
              olcAccess = [
                "{0}to attrs=userPassword by self write by dn.base=\"cn=${ldapRootUser},${dbSuffix}\" write by anonymous auth by * none"
                "{1}to attrs=krbPrincipalKey by anonymous auth by dn.exact=\"uid=kdc,ou=services,${dbSuffix}\" read by dn.exact=\"uid=kadmin,ou=services,${dbSuffix}\" write by self write by * none"
                "{2}to dn.subtree=\"ou=services,${dbSuffix}\" by dn.exact=\"uid=kdc,ou=services,${dbSuffix}\" read by dn.exact=\"uid=kadmin,ou=services,${dbSuffix}\" write by * none"
                "{3}to * by dn.base=\"cn=${ldapRootUser},${dbSuffix}\" write by self write by * read"
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
                "{0}to * by dn.base=\"gidNumber=0+uidNumber=0,cn=peercred,cn=external,cn=auth\" read by dn.base=\"cn=${ldapRootUser},${dbSuffix}\" read by * none"
              ];
            };
          };
        };
      };
      
      # Declarative contents remain the same but reference templates for passwords
      declarativeContents = {
        ${dbSuffix} = ''
          dn: ${dbSuffix}
          objectClass: top
          objectClass: dcObject
          objectClass: organization
          o: ${config.networking.domain}
          
          dn: ou=services,${dbSuffix}
          objectClass: top
          objectClass: organizationalUnit
          
          # Service accounts will get passwords from templates
          # The actual LDIF generation would be handled by a systemd service
          # that reads from the template files
        '';
      };
      
      mutableConfig = false;
    };
  };
  
  # SystemD service to inject passwords into LDAP on startup
  systemd.services.openldap-inject-passwords = {
    description = "Inject passwords into OpenLDAP";
    wantedBy = [ "openldap.service" ];
    after = [ "openldap.service" ];
    
    script = ''
      # Wait for LDAP to be ready
      sleep 5
      
      # Read passwords from sops
      KDC_PASS=$(cat ${config.sops.secrets."openldap/kdc-password".path})
      KADMIN_PASS=$(cat ${config.sops.secrets."openldap/kadmin-password".path})
      
      # Update service account passwords
      ldapmodify -x -D "cn=${ldapRootUser},${dbSuffix}" -w "$(cat ${config.sops.secrets."openldap/admin-password".path})" <<EOF
      dn: uid=kdc,ou=services,${dbSuffix}
      changetype: modify
      replace: userPassword
      userPassword: $KDC_PASS
      
      dn: uid=kadmin,ou=services,${dbSuffix}
      changetype: modify
      replace: userPassword
      userPassword: $KADMIN_PASS
      EOF
    '';
    
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
  };
}