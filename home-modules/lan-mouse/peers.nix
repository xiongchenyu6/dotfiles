# Hosts sharing one keyboard/mouse via lan-mouse. Consumed by both the
# home-manager side (daemon + client layout, ./default.nix) and the NixOS side
# (firewall, nixos-modules/lan-mouse.nix). Fingerprint = sha256 of the host's
# ~/.config/lan-mouse/lan-mouse.pem, which the daemon generates on first start.
{
  # Layout, left to right: game | gz-pc
  peers = {
    game = {
      # eno1 (wired, preferred) first, then WiFi; both are DHCP leases.
      ips = [
        "192.168.65.43"
        "192.168.64.245"
      ];
      fingerprint = "66:1c:4b:12:e2:4a:b1:b0:8f:2a:fe:c9:99:2a:02:9c:e0:25:aa:46:7d:27:c8:39:66:45:ec:be:23:13:0d:3e";
    };
    gz-pc = {
      ips = [ "192.168.65.53" ];
      fingerprint = "31:29:c3:4a:d3:8e:50:1e:57:a1:95:99:2c:17:24:fe:c9:85:31:00:d9:55:ee:c0:6f:e5:0d:61:b5:ad:4d:bf";
    };
  };
  layout = {
    game.gz-pc = "right";
    gz-pc.game = "left";
  };
}
