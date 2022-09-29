# Edit this configuration file to define what should be installed on


# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, symlinkJoin, domain, ... }:
let
  share = import ../common/share.nix;
in
{
  krb5 = {
    enable = true;
    realms = {
      "FREEMAN.ENGINEER" = {
        admin_server = "freeman.engineer";
        kdc = "freeman.engineer";
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
    plugins = {
      ldap_util = { };
    };
    extraConfig = ''
      [dbmodules]
          openldap_ldapconf = {
              db_module_dir = ${pkgs.krb5Full}/lib/krb5/plugins/kdb/
              db_library = kldap
              ldap_servers = ldap://freeman.engineer:389
              ldap_kerberos_container_dn = cn=kerberos,dc=freeman,dc=engineer
              ldap_kdc_dn = cn=admin,dc=freeman,dc=engineer
              ldap_kadmind_dn = cn=admin,dc=freeman,dc=engineer
              ldap_service_password_file = ${./passwordFile}
              ldap_conns_per_server = 5
       }
    '';
  };

  networking = {
    domain = "freeman.engineer";
  };

  environment = {
    systemPackages = with pkgs; [
      # self.packages."${system}".bttc
      dig
      git
      wireguard-tools
      traceroute
      python3
      inetutils
      killall
      tree
      tmux
      vim
      tcpdump
      file
      schema2ldif
      cyrus_sasl
    ];
  };

  services =
    let
      dbDomain = "freeman.engineer";
      dbSuffix = "dc=freeman,dc=engineer";
      testUser = "test";
      ldapRootUser = "admin";
      ldapRootPassword = "a";
    in
    {
      openldap =
        {
          enable = true;
          settings =
            {
              attrs = {
                olcLogLevel = [ "-1" ];
                olcAuthzRegexp = "uid=([^,]*),cn=gssapi,cn=auth uid=\$1,ou=accounts,ou=posix,${dbSuffix}";
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
                    ./kerberos.ldif
                  ];
                };
                "olcDatabase={1}mdb" = {
                  attrs = {
                    objectClass = [ "olcDatabaseConfig" "olcMdbConfig" ];
                    olcDbIndex = "krbPrincipalName eq,pres,sub";
                    olcDatabase = "{1}mdb";
                    olcDbDirectory = "/var/lib/openldap/ldap";
                    olcSuffix = "${dbSuffix}";
                    olcRootDN = "cn=${ldapRootUser},${dbSuffix}";
                    olcRootPW = "${ldapRootPassword}";
                    olcAccess = [
                      "{0}to attrs=userPassword by self write by dn.base=\"cn=${ldapRootUser},${dbSuffix}\" write by anonymous auth by * none"
                      "{1}to * by dn.base=\"cn=${ldapRootUser},${dbSuffix}\" write by self write by * read"
                      "{2}to attrs=krbPrincipalKey by anonymous auth by dn.exact=\"uid=kdc-service,${dbSuffix}\" read by dn.exact=\"uid=kadmin-service,${dbSuffix}\" write by self write by * none"
                      "{3}to dn.subtree=\"cn=krbContainer,dc=${dbSuffix}\"
                by dn.exact=\"uid=kdc-service,${dbSuffix}\" read
                by dn.exact=\"uid=kadmin-service,${dbSuffix}\" write
                by * none"
                    ];
                  };
                };
              };
            };
          declarativeContents = {
            ${dbSuffix} = ''
              dn: uid=kdc-service,dc=freeman,dc=engineer
              uid: kdc-service
              objectClass: account
              objectClass: simpleSecurityObject
              userPassword: x
              description: Account used for the Kerberos KDC

              dn: uid=kadmin-service,dc=freeman,dc=engineer
              uid: kadmin-service
              objectClass: account
              objectClass: simpleSecurityObject
              userPassword: x
              description: Account used for the Kerberos Admin server

              dn: ${dbSuffix}
              objectClass: top
              objectClass: dcObject
              objectClass: organization
              o: ${dbDomain}

              dn: ou=developers,${dbSuffix}
              objectClass: top
              objectClass: organizationalUnit

              dn: uid=${testUser},ou=developers,${dbSuffix}
              objectClass: person
              objectClass: posixAccount
              homeDirectory: /home/${testUser}
              uidNumber: 1234
              gidNumber: 1234
              cn: ""
              sn: ""

            '';
          };
          mutableConfig = false;
        };

      sssd = {
        enable = true;
        environmentFile = "${pkgs.writeText "ldap-root" "LDAP_BIND_PW=${ldapRootPassword}"}";
        config = ''
          [sssd]
          config_file_version = 2
          services = nss, pam, ssh, autofs
          domains = ${dbDomain}
          [nss]
          [pam]

          [domain/${dbDomain}]
          autofs_provider = ldap
          ldap_schema = rfc2307bis
          id_provider = ldap
          ldap_uri = ldap://localhost:389
          ldap_search_base = ${dbSuffix}
          ldap_default_bind_dn = cn=${ldapRootUser},${dbSuffix}
          ldap_default_authtok_type = password
          ldap_default_authtok = $LDAP_BIND_PW

          auth_provider = krb5
          krb5_realm = FREEMAN.ENGINEER
          krb5_server = ${dbDomain}
          krb5_validate = true
          debug_level = 9
        '';
      };
      openssh = {
        enable = true;
        startWhenNeeded = false;
        #useDns = true;
        extraConfig = ''
          GSSAPIAuthentication yes
          GSSAPICleanupCredentials yes
        '';
      };
      saslauthd = {
        enable = true;
        mechanism = "kerberos5";
        config = ''
        '';
      };
    };

  users = {
    defaultUserShell = pkgs.zsh;
    # Define a user account. Don't forget to set a password with ‘passwd’.
    users = {
      freeman = {
        shell = pkgs.zsh;
        isNormalUser = true;
        description = "freeman";
        group = "users";
        openssh.authorizedKeys.keys = [
          share.freeman.user.public-key
        ];
        extraGroups = [
          "networkmanager"
          "wheel"
          "video"
          "audio"
          "cdrom"
          "disk"
          "floppy"
          "scanner"
          "storage"
          "power"
          "dialout"
          "plugdev"
          "lp"
          "input"
          "docker"
          "socket"
          "spi"
          "bus"
          "dropbox"
        ];
      };
    };
  };

  security.pam = {
    krb5.enable = false;
    services = {
      sshd = {
        makeHomeDir = true;
      };
    };
  };
  # Allow unfree packages
  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = true;
    };
  };

}
