{pkgs, ...}: {
  services = {
    saslauthd = {
      enable = true;
      mechanism = "kerberos5";
      package = pkgs.cyrus_sasl_with_ldap;
    };
  };
}
