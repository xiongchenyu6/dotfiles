# Edit
{ config, lib, pkgs, ... }: {
  imports = [
    ../../../profiles/wsl.nix
    ../../../profiles/core/nixos.nix
    ../../../users/root/nixos.nix
    ../../../profiles/dvorak.nix
    ../../../users/freeman.xiong
    ../../../profiles/sops.nix
    ../../../profiles/common/apps/kerberos.nix
    ../../../profiles/client/cli/nixos.nix
  ];

  sops.secrets."wireguard/office" = { };

  services.openssh.enable = true;
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
        imports = [ ../../../users/profiles/gui/stow-config.nix ];
        sops = { gnupg = { home = "~/.gnupg"; }; };
        xdg = {
          mimeApps = {
            defaultApplications = {
              "text/html" = "wslview";
              "x-scheme-handler/http" = "wslview";
              "x-scheme-handler/https" = "wslview";
              "x-scheme-handler/about" = "wslview";
              "x-scheme-handler/unknown" = "wslview";
            };
          };
        };
        home = lib.mkIf pkgs.stdenv.isLinux {
          sessionVariables = { EDITOR = "code --wait"; };
        };
      };
    };
  };
}
