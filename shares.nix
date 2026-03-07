{ lib, ... }:
with builtins;
let
  shares = lib.importTOML ./shares.toml;
in
rec {

  inherit (shares) users;

  inherit (shares) hosts;

  inherit (shares) root-cas;

  inherit (shares) oauth;

  users-dict = listToAttrs (
    map (u: {
      name = "${u.gn}.${toString u.sn}";
      value = u;
    }) users
  );

  hosts-dict = listToAttrs (
    map (h: {
      name = h.host;
      value = h;
    }) hosts
  );
}
