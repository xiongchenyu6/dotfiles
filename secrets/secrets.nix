let
  /* edit permissions */
  share = (import ../common/share.nix);
  freeman = share.freeman.user.public-key;
  users = [ freeman ];

  /* deploy permissions */
  laptop = share.freeman.system.public-key;
  tc = share.tc.system.public-key;
  systems = [ laptop tc ];
in
{
  "freeman_wg_pk.age".publicKeys = [ freeman laptop ];
  "tc_wg_pk.age".publicKeys = [ freeman tc ];
  "tc_https_pk.age".publicKeys = [ freeman tc ];
}
