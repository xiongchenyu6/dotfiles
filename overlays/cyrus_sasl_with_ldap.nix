final: prev: {
  cyrus_sasl_with_ldap =
    (prev.cyrus_sasl.override { enableLdap = true; }).overrideAttrs (old: {
      postInstall = ''
        ln -sf ${prev.ldap-passthrough-conf}/slapd.conf $out/lib/sasl2/
        ln -sf ${prev.ldap-passthrough-conf}/smtpd.conf $out/lib/sasl2/
      '';
    });
}
