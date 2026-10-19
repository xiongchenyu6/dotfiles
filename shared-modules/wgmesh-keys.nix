# This repo's keys for wgmesh: each host reuses its `wireguard/<host>` key.
{ config, ... }:
{
  sops.secrets = {
    "wireguard/${config.networking.hostName}" = { };
    "wgmesh/gossip-secret" = { };
  };

  services.wgmesh = {
    privateKeyFile = config.sops.secrets."wireguard/${config.networking.hostName}".path;
    gossipSecretFile = config.sops.secrets."wgmesh/gossip-secret".path;
  };
}
