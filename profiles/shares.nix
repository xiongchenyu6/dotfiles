{ lib, ... }:
with builtins;
let shares = lib.importTOML ./shares.toml;
in rec {

  users = shares.users;

  groups = shares.groups;

  hosts = shares.hosts;

  root-cas = shares.root-cas;

  users-dict = listToAttrs (map (u: {
    name = "${u.gn}.${toString u.sn}";
    value = u;
  }) users);

  groups-dict = listToAttrs (map (g: {
    name = g.name;
    value = g;
  }) groups);

  hosts-dict = listToAttrs (map (h: {
    name = h.host;
    value = h;
  }) hosts);
}