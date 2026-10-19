# wgmesh settings shared by NixOS (nixos-modules/wgmesh.nix) and nix-darwin
# (darwin-modules/wgmesh.nix): IPv4 of our DN42 /27 between our own hosts,
# direct where hole punching works and relayed through oracle-amd-002 (the hub
# and DN42 border) otherwise. Members are the shares.toml hosts with a
# `mesh-ip`; each reuses its existing `wireguard/<host>` key.
{
  config,
  lib,
  shares,
  ...
}:
{
  sops.secrets = {
    "wireguard/${config.networking.hostName}" = { };
    "wgmesh/gossip-secret" = { };
  };

  services.wgmesh = {
    enable = true;
    cidr = "172.22.240.96/27";
    relay = "oracle-amd-002";
    # DN42 IPv4, reached through the border router.
    relayPrefixes = [ "172.20.0.0/14" ];
    # 51820 is netbird's (wt0) on the desktops.
    listenPort = 51821;
    privateKeyFile = config.sops.secrets."wireguard/${config.networking.hostName}".path;
    gossipSecretFile = config.sops.secrets."wgmesh/gossip-secret".path;
    nodes = lib.mapAttrs (name: host: {
      address = host.wg.mesh-ip;
      publicKey = host.wg.public-key;
      endpoint = if name == "oracle-amd-002" then "213.35.117.232:51821" else null;
    }) (lib.filterAttrs (_: host: host ? wg && host.wg ? mesh-ip) shares.hosts);
  };
}
