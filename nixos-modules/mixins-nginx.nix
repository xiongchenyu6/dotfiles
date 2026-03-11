# Drop-in replacement for srvos.nixosModules.mixins-nginx
# The srvos version is incompatible with current nixpkgs due to infinite recursion
# between sslDhparam and security.dhparams.params.nginx.
# This uses the new nixpkgs `sslDhparam = true` API instead.
{ config, lib, ... }:
{
  networking.firewall.allowedTCPPorts = [ 443 80 ];

  services.nginx = {
    enable = true;

    statusPage = lib.mkDefault true;
    recommendedBrotliSettings = lib.mkDefault true;
    recommendedGzipSettings = lib.mkDefault true;
    recommendedOptimisation = lib.mkDefault true;
    recommendedProxySettings = lib.mkDefault true;
    recommendedTlsSettings = lib.mkDefault true;

    commonHttpConfig = "access_log syslog:server=unix:/dev/log;";

    resolver.addresses =
      let
        isIPv6 = addr: builtins.match ".*:.*:.*" addr != null;
        escapeIPv6 = addr: if isIPv6 addr then "[${addr}]" else addr;
        cloudflare = [
          "1.1.1.1"
          "2606:4700:4700::1111"
        ];
        resolvers =
          if config.networking.nameservers == [ ] then cloudflare else config.networking.nameservers;
      in
      map escapeIPv6 resolvers;

    sslDhparam = true;
  };
}
