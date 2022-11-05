_: {
  security = {
    rtkit = { enable = true; };
    sudo = {
      enable = true;
      wheelNeedsPassword = true;
    };
    acme = { acceptTerms = true; };
    pki = let
      share = import ../../common/share.nix;
      pam = { krb5.enable = false; };
    in {
      certificates = [ share.dn42.root-ca share.digitcert-global.root-ca ];
    };
  };
}
