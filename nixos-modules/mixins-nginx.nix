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

    commonHttpConfig = ''
      access_log syslog:server=unix:/dev/log;

      # Restore real client IP when behind the Cloudflare proxy. Without this
      # nginx logs (and any app reading X-Forwarded-For) see Cloudflare edge
      # IPs instead of the originating client. Refresh periodically from
      # https://www.cloudflare.com/ips/.
      set_real_ip_from 173.245.48.0/20;
      set_real_ip_from 103.21.244.0/22;
      set_real_ip_from 103.22.200.0/22;
      set_real_ip_from 103.31.4.0/22;
      set_real_ip_from 141.101.64.0/18;
      set_real_ip_from 108.162.192.0/18;
      set_real_ip_from 190.93.240.0/20;
      set_real_ip_from 188.114.96.0/20;
      set_real_ip_from 197.234.240.0/22;
      set_real_ip_from 198.41.128.0/17;
      set_real_ip_from 162.158.0.0/15;
      set_real_ip_from 104.16.0.0/13;
      set_real_ip_from 104.24.0.0/14;
      set_real_ip_from 172.64.0.0/13;
      set_real_ip_from 131.0.72.0/22;
      set_real_ip_from 2400:cb00::/32;
      set_real_ip_from 2606:4700::/32;
      set_real_ip_from 2803:f800::/32;
      set_real_ip_from 2405:b500::/32;
      set_real_ip_from 2405:8100::/32;
      set_real_ip_from 2a06:98c0::/29;
      set_real_ip_from 2c0f:f248::/32;
      real_ip_header CF-Connecting-IP;
      real_ip_recursive on;
    '';

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
