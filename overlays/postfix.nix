final: prev: {
  postfix = prev.postfix.override {cyrus_sasl = final.cyrus_sasl_with_ldap;};
}
