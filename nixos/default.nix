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
  system.nixos.tags = [ "freeman" ];

  krb5 = {
    enable = true;
    realms = {
      "FREEMAN.ENGINEER" = {
        admin_server = "freeman.engineer";
        kdc = [ "freeman.engineer" "mail.freeman.engineer" ];
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
          ldap_schema = rfc2307
          id_provider = ldap
          sudo_provider = ldap
          ldap_uri = ldaps://freeman.engineer
          ldap_search_base = ${dbSuffix}
          ldap_default_bind_dn = cn=${ldapRootUser},${dbSuffix}
          ldap_sudo_search_base = ou=SUDOers,${dbSuffix}
          ldap_sasl_mech = GSSAPI
          ldap_sasl_authid = host/${dbDomain}
          ldap_sasl_realm = FREEMAN.ENGINEER

          chpass_provider = krb5

          use_fully_qualified_names = false

          auth_provider = krb5
          access_provider = simple

          krb5_realm = FREEMAN.ENGINEER
          krb5_server = ${dbDomain}
          krb5_validate = true
          debug_level = 0x3ff0
        '';
      };
      openssh = {
        enable = true;
        banner = ''
          Welcome to the NixOS machine
        '';
        startWhenNeeded = false;
        useDns = true;
        extraConfig = ''
          GSSAPIAuthentication yes
          UsePAM yes
          GSSAPICleanupCredentials yes
          PasswordAuthentication yes
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

  security = {
    sudo = {
      extraRules = [
        {
          groups = [ "developers" ];
          commands = [ "ALL" ];
        }
      ];
    };
    pam = {
      krb5.enable = false;
      services = {
        sshd = {
          makeHomeDir = true;
        };
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
