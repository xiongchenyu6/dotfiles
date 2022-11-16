{
  profiles,
  pkgs,
  ...
}: {
  security = {
    rtkit = {enable = true;};
    sudo = {
      enable = true;
      # wheelNeedsPassword = false;
      package = pkgs.sudo.override {
        withInsults = true;
        withSssd = true;
      };
    };
    acme = {acceptTerms = true;};
    pam = {krb5.enable = true;};
    pki = {
      certificates = [profiles.share.dn42.root-ca profiles.share.digitcert-global.root-ca];
    };
  };
}
