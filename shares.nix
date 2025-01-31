{ lib, ... }:
with builtins;
let shares = lib.importTOML ./shares.toml;
in rec {

  inherit (shares) users;

  inherit (shares) groups;

  inherit (shares) hosts;

  inherit (shares) root-cas;

  users-dict = listToAttrs (map (u: {
    name = "${u.gn}.${toString u.sn}";
    value = u;
  }) users);

  groups-dict = listToAttrs (map (g: {
    inherit (g) name;
    value = g;
  }) groups);

  hosts-dict = listToAttrs (map (h: {
    name = h.host;
    value = h;
  }) hosts);
}
