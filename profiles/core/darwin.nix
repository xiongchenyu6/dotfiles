# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{pkgs, ...}: {
  environment.systemPackages = with pkgs; [krb5 openssh_gssapi];

  imports = [./common.nix];
  services.nix-daemon.enable = true;
  system = {stateVersion = 4;}; # Did you read the comment?
  nix = {
    settings = {auto-optimise-store = true;};
    package = pkgs.nix;
  };
  security.pam.enableSudoTouchIdAuth = true;

  system.darwinLabel = "with-gui";
  environment.etc = {
    "krb5.conf" = {
      enable = true;
      text = ''
        [libdefaults]
          default_realm = TRONTECH.LINK
          dns_fallback = true
          dns_lookup_kdc = true
          dns_lookup_realm = true
          ignore_acceptor_hostname = true
          rdns = false

        [realms]
          TRONTECH.LINK = {
            admin_server = admin.inner.trontech.link
            database_module = openldap_ldapconf
            default_domain = admin.inner.trontech.link
            kdc = admin.inner.trontech.link
            kpasswd_server = admin.inner.trontech.link
          }

        [domain_realm]
          .inner.trontech.link = TRONTECH.LINK
          .trontech.link = TRONTECH.LINK
          trontech.link = TRONTECH.LINK
      '';
    };
  };
}
