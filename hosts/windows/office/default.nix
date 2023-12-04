# Edit
{ config, lib, profiles, pkgs, mylib, ... }: {
  imports = [
    ../../../profiles/core/nixos.nix
    ../../../users/root/nixos.nix
    ../../../profiles/dvorak.nix
    ../../../users/freeman.xiong
    ../../../profiles/sops.nix
    ../../../profiles/common/apps/kerberos.nix
    ../../../profiles/client/cli/nixos.nix
  ];

  wsl = {
    enable = true;
    defaultUser = "freeman.xiong";
  };
  services.openssh.enable = true;

  sops.secrets."wireguard/office" = { };

  nixpkgs = {
    config = {
      permittedInsecurePackages =
        [ "openssl-1.1.1w" "electron-19.1.9" "zotero-6.0.27" ];
      allowBroken = true;
    };
  };
  krb5 = {
    realms = let
      tronRealm = "TRONTECH.LINK";
      tronDomain = "trontech.link";
    in {
      "${tronRealm}" = {
        admin_server = "admin.inner.${tronDomain}";
        kdc = [ "admin.inner.${tronDomain}" ];
        default_domain = "admin.inner.${tronDomain}";
        kpasswd_server = "admin.inner.${tronDomain}";
        database_module = "openldap_ldapconf";
      };
      domain_realm = {
        "${tronDomain}" = tronRealm;
        ".inner.${tronDomain}" = tronRealm;
        ".${tronDomain}" = tronRealm;
      };
    };
  };

  home-manager = {
    users = {
      "freeman.xiong" = {
        sops = {
          gnupg = { home = "~/.gnupg"; };
          secrets = {
            # The path to the file to decrypt.
            gptcommit = {
              name = "gptcommit";
              path = "/home/freeman.xiong/.config/gptcommit/config.toml";
              mode = "777";
            };
          };
        };
      };
    };
  };
}
