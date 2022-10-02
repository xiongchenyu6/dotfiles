# Edit this configuration file to define what should be installed on


# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, symlinkJoin, domain, ... }:
let
  share = import ../common/share.nix;
  dbDomain = "freeman.engineer";
  realm = "FREEMAN.ENGINEER";
  dbSuffix = "dc=freeman,dc=engineer";
  defaultUser = "freeman";
  ldapRootUser = "admin";
  kerberosLdapPassword = "a";
in
{
  krb5 = {
    enable = true;
    realms = {
      "FREEMAN.ENGINEER" = {
        admin_server = "freeman.engineer";
        kdc = "freeman.engineer";
        default_domain = "freeman.enginer";
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
              ldap_kerberos_container_dn = cn=krbContainer,ou=services,${dbSuffix}
              ldap_kdc_dn = uid=kdc,ou=services,${dbSuffix}
              ldap_kadmind_dn = uid=kadmin,ou=services,${dbSuffix}
              ldap_service_password_file = ${../common/passwordFile}
              ldap_conns_per_server = 5
       }
    '';
  };

  networking = {
    inherit domain;
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
                  olcLogLevel = [ "-1" ];
                  olcSaslHost = "freeman.engineer";
                  olcTLSCACertificateFile = credsDir + "/full.pem";
                  olcTLSCertificateFile = credsDir + "/cert.pem";
                  olcTLSCertificateKeyFile = credsDir + "/key.pem";
                  olcAuthzRegexp = [
                    "{0}uid=${ldapRootUser},cn=gssapi,cn=auth cn=${ldapRootUser},${dbSuffix}"
                    "{1}uid=([^,]*),cn=gssapi,cn=auth uid=\$1,ou=accounts,ou=posix,${dbSuffix}"
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
                    ../common/kerberos.ldif
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
              objectClass: simpleSecurityObject
              objectClass: organizationalUnit
              userpassword: {SASL}${developers}@${realm}

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
            '';
          };
          mutableConfig = false;
        };

      sssd = {
        enable = true;
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
          ldap_sasl_mech = GSSAPI

          auth_provider = krb5
          krb5_realm = FREEMAN.ENGINEER
          krb5_server = ${dbDomain}
          krb5_validate = true
          debug_level = 9
        '';
      };
      openssh = {
        enable = true;
        banner = ''
          Welcome to the NixOS machine
        '';
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
        package = pkgs.cyrus_sasl_with_ldap;
      };
      postfix = {
        inherit domain;
        enable = true;
      };

    };

  users = {
    # Define a user account. Don't forget to set a password with ‘passwd’.
    users = {
      freeman = {
        isNormalUser = true;
        description = "freeman";
        group = "users";
        openssh.authorizedKeys.keys = [
          share.office.user.public-key
        ];
        shell = pkgs.zsh;
        extraGroups = [
          "networkmanager"
          "wheel"
          "video"
          "audio"
          "cdrom"
          "disk"
          "floppy"
          "dialout"
          "lp"
          "input"
          "docker"
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

  home-manager = {
    useGlobalPkgs = true;
    useUserPackages = true;
    users = {
      freeman = {
        home = {
          stateVersion = "22.11";
        };
        imports = [ ../home/cli ];
      };

    };
  };

  nix = {
    settings = {
      experimental-features = [ "nix-command" "flakes" "repl-flake" ];
      trusted-users = [ "root" "freeman" ];
    };
  };
}
