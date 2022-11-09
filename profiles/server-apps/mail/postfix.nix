{ config, ... }:

{
  services = let
    credsDir = config.security.acme.certs."${config.networking.fqdn}".directory;
  in {
    postfix = {
      inherit (config.networking) domain;
      enable = true;
      enableSubmission = true;
      enableSubmissions = true;

      sslCert = credsDir + "/cert.pem";
      sslKey = credsDir + "/key.pem";

      submissionsOptions = {
        smtpd_sasl_auth_enable = "yes";
        smtpd_sasl_type = "cyrus";
        smtpd_sasl_path = "smtpd";
        smtpd_sasl_security_options = "noanonymous";
        broken_sasl_auth_clients = "yes";

        smtpd_tls_security_level = "encrypt";
        smtpd_client_restrictions = "permit_sasl_authenticated,reject";
        # smtpd_sender_login_maps = "hash:/etc/postfix/vaccounts";
        # smtpd_sender_restrictions = "reject_sender_login_mismatch";
        # smtpd_recipient_restrictions = "reject_non_fqdn_recipient,reject_unknown_recipient_domain,permit_sasl_authenticated,reject";

      };
      config = {
        smtpd_sasl_auth_enable = "yes";
        smtpd_sasl_type = "cyrus";
        smtpd_sasl_path = "smtpd";
        smtpd_sasl_security_options = "noanonymous";
        broken_sasl_auth_clients = "yes";

        tls_preempt_cipherlist = true;
        smtpd_tls_auth_only = true;
        tls_random_source = "dev:/dev/urandom";
        smtpd_tls_security_level = "may";

        # strong might suffice and is computationally less expensive
        smtpd_tls_eecdh_grade = "ultra";

        # Disable obselete protocols
        smtpd_tls_protocols =
          "TLSv1.3, TLSv1.2, TLSv1.1, !TLSv1, !SSLv2, !SSLv3";
        smtp_tls_protocols =
          "TLSv1.3, TLSv1.2, TLSv1.1, !TLSv1, !SSLv2, !SSLv3";
        smtpd_tls_mandatory_protocols =
          "TLSv1.3, TLSv1.2, TLSv1.1, !TLSv1, !SSLv2, !SSLv3";
        smtp_tls_mandatory_protocols =
          "TLSv1.3, TLSv1.2, TLSv1.1, !TLSv1, !SSLv2, !SSLv3";

        smtp_tls_ciphers = "high";
        smtpd_tls_ciphers = "high";
        smtp_tls_mandatory_ciphers = "high";
        smtpd_tls_mandatory_ciphers = "high";

        # Disable deprecated ciphers
        smtpd_tls_mandatory_exclude_ciphers =
          "MD5, DES, ADH, RC4, PSD, SRP, 3DES, eNULL, aNULL";
        smtpd_tls_exclude_ciphers =
          "MD5, DES, ADH, RC4, PSD, SRP, 3DES, eNULL, aNULL";
        smtp_tls_mandatory_exclude_ciphers =
          "MD5, DES, ADH, RC4, PSD, SRP, 3DES, eNULL, aNULL";
        smtp_tls_exclude_ciphers =
          "MD5, DES, ADH, RC4, PSD, SRP, 3DES, eNULL, aNULL";
      };
    };
  };
}
