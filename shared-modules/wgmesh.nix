# wgmesh membership: IPv4 of our DN42 /27 between our own hosts, direct where
# hole punching works and relayed through oracle-amd-002 (the hub and DN42
# border) otherwise. Members are the shares.toml hosts with a `mesh-ip`.
#
# No secrets here, so other flakes (AutoLifeRobot/nixos for sg-office) import
# this file by path and set privateKeyFile/gossipSecretFile themselves. In
# this repo that is shared-modules/wgmesh-keys.nix.
{ lib, ... }:
let
  shares = import ./shares.nix { inherit lib; };
in
{
  services.wgmesh = {
    enable = true;
    cidr = "172.22.240.96/27";
    relay = "oracle-amd-002";
    # DN42 IPv4, reached through the border router.
    relayPrefixes = [ "172.20.0.0/14" ];
    # 51820 is netbird's (wt0) on the desktops.
    listenPort = 51821;
    nodes = lib.mapAttrs (_: host: {
      address = host.wg.mesh-ip;
      publicKey = host.wg.public-key;
      endpoint = host.wg.mesh-endpoint or null;
    }) (lib.filterAttrs (_: host: host ? wg && host.wg ? mesh-ip) shares.hosts);
  };
}
