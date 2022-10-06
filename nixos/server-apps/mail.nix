{ config, pkgs, lib, symlinkJoin, domain, ... }:

{
  services =
    {
      postfix = {
        inherit domain;
        enable = true;
        enableSubmission = true;
        submissionsOptions = {
          smtpd_sasl_auth_enable = "yes";
          smtpd_sasl_type = "cyrus";
          smtpd_sasl_path = "smtpd";
          smtpd_sasl_security_options = "noanonymous";
          broken_sasl_auth_clients = "yes";
        };
        config = {
          smtpd_sasl_auth_enable = "yes";
          smtpd_sasl_type = "cyrus";
          smtpd_sasl_path = "smtpd";
          smtpd_sasl_security_options = "noanonymous";
          broken_sasl_auth_clients = "yes";
        };
        # extraMasterConf = ''
        #   smtp        inet  n        -       -       -        -        smtpd -v
        # '';
      };
    };
}
