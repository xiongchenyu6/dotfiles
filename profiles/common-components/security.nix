{ profiles, pkgs, config, ... }: {
  sops.secrets."oath/seed" = { };
  security = {
    rtkit = { enable = true; };
    sudo = {
      enable = true;
      # wheelNeedsPassword = false;
      package = pkgs.sudo.override {
        withInsults = true;
        withSssd = true;
      };
    };
    acme = { acceptTerms = true; };
    pam = {
      krb5.enable = false;
      oath = {
        enable = false;
        usersFile = config.sops.secrets."oath/seed".path;
        window = 30;
      };
    };
    pki = {
      certificates =
        [ profiles.share.dn42.root-ca profiles.share.digitcert-global.root-ca ];
    };
  };
}
