# IPv4 of our DN42 /27 between the NixOS hosts: direct WireGuard where hole
# punching works, relayed through oracle-amd-002 (the hub and DN42 border)
# otherwise. Members are the shares.toml hosts with a `mesh-ip`; each reuses its
# existing `wireguard/<host>` key. IPv6 stays on wg_ora (wgautomesh is IPv4-only).
{
  config,
  inputs,
  lib,
  shares,
  ...
}:
{
  imports = [ inputs.wgmesh.nixosModules.default ];

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
