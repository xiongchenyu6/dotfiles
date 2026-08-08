# Parses shares.toml into the `shares` global arg (ezConfigs.globalArgs in
# flake.nix). The TOML is keyed directly by user/host name, so no reshaping
# is needed — consumers use e.g. shares.users."freeman.xiong".public-key,
# shares.hosts.game.wg.public-key, attrValues shares.root-cas.
{ lib, ... }:
lib.importTOML ./shares.toml
