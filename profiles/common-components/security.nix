_: {
  security = {
    rtkit = { enable = true; };
    sudo = {
      enable = true;
      wheelNeedsPassword = false;
    };
    acme = { acceptTerms = true; };
    pam = { krb5.enable = true; };
    pki = let share = import ../../common/share.nix;
    in {
      certificates = [ share.dn42.root-ca share.digitcert-global.root-ca ];
    };
  };
}
