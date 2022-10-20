{ config, pkgs, options, lib, ... }:
let share = import ../../common/share.nix;
in {
  security = {
    rtkit = { enable = true; };
    sudo = { enable = true; };
    acme = { acceptTerms = true; };
    pki = {
      certificates = [ share.dn42.root-ca share.digitcert-global.root-ca ];
    };
  };

}
