# WireGuard mesh client for macOS hosts: wg-quick on wireguard-go, run by
# launchd. Each host sets its own `address` list and `privateKeyFile` (sops)
# on `networking.wg-quick.interfaces.wg_ora`; the hub peer lives here.
{ pkgs, shares, ... }:
{
  environment.systemPackages = with pkgs; [
    wireguard-tools
    wireguard-go # WireGuard userspace implementation for macOS
  ];

  networking.wg-quick.interfaces.wg_ora = {
    # Routes for all allowedIPs are created by wg-quick.
    peers = [
      {
        publicKey = shares.hosts.oracle-amd-002.wg.public-key;
        endpoint = "213.35.117.232:22616";
        persistentKeepalive = 30;
        allowedIPs = [
          "10.0.0.0/8"
          "172.20.0.0/14"
          "172.31.0.0/16"
          "fd00::/8"
          "fe80::/10"
          "fd48:4b4:f3::/48"
          "ff02::1:6/128"
          "224.0.0.251/32"
          "ff02::fb/128"
        ];
      }
    ];

    # Auto-start the interface
    autostart = true;
  };

  networking.knownNetworkServices = [ "Wi-Fi" ];
}
