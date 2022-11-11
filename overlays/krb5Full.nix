_: prev: {
  krb5Full =
    prev.krb5Full.overrideAttrs
    (old: {configureFlags = old.configureFlags ++ ["--with-ldap"];});
}
