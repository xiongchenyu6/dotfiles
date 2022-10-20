let
  # edit permissions
  share = import ../share.nix;
  office = share.office.user.public-key;
  users = [ office ];

  # deploy permissions
  laptop = share.office.system.public-key;
  tc = share.tc.system.public-key;
  systems = [ laptop tc ];
in {
  "office_wg_pk.age".publicKeys = [ office laptop ];
  "tc_wg_pk.age".publicKeys = [ office tc ];
  "tc_https_pk.age".publicKeys = [ office tc ];
  "acme_credentials.age".publicKeys = [ office tc ];
  "ldap_credentials.age".publicKeys = [ office tc ];
  "ldap-password.age".publicKeys = [ office tc ];
  "gitea_postgres.age".publicKeys = [ office tc ];
}

