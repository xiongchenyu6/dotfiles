_final: prev: {

  libkrb5 = prev.krb5.override { type = "lib"; };

  krb5 = prev.krb5.overrideAttrs
    (old: { configureFlags = old.configureFlags ++ [ "--with-ldap" ]; });

}
