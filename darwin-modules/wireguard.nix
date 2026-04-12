{
  config,
  pkgs,
  lib,
  shares,
  ...
}:
let
  wgInterface = "wg_ora";
in
{
  sops.secrets."wireguard/office" = { };

  environment.systemPackages = with pkgs; [
    wireguard-tools
    wireguard-go # WireGuard userspace implementation for macOS
  ];

  # Use the built-in networking.wg-quick.interfaces for WireGuard configuration
  networking.wg-quick.interfaces.wg_ora = {
    # Private key from sops secrets
    privateKeyFile = config.sops.secrets."wireguard/office".path;

    # Interface addresses
    address = [
      "fe80::101/64"
      "172.22.240.98/32"
      "fd48:4b4:f3::2/128"
    ];

    # Let WireGuard manage routes automatically (default behavior)
    # Routes will be created for all allowedIPs

    # Peer configuration
    peers = [
      {
        publicKey = shares.hosts-dict.oracle-amd-002.wg.public-key;
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
