final: prev: {
  cyrus_sasl_with_ldap =
    (prev.cyrus_sasl.override { enableLdap = true; }).overrideAttrs (_: {
      postInstall = ''
        ln -sf ${prev.ldap-passthrough-conf}/slapd.conf $out/lib/sasl2/
        ln -sf ${prev.ldap-passthrough-conf}/smtpd.conf $out/lib/sasl2/
      '';
    });
  openldap_with_cyrus_sasl = (prev.openldap.overrideAttrs (old: {
    configureFlags = old.configureFlags
      ++ [ "--enable-spasswd" "--with-cyrus-sasl" ];
    doCheck = false;
  })).override { cyrus_sasl = final.cyrus_sasl_with_ldap; };
}
