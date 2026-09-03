# Software KVM between the workstations: one keyboard/mouse, switch by pushing
# the pointer across the screen edge. Module lives in NUR (lan-mouse has no
# nixpkgs/home-manager module; services.synergy is X11-only and cannot capture
# input under niri).
#
# Both sides run the same daemon; each host lists the other as a client on the
# matching side. Peers are pinned by IP because lan-mouse resolves hostnames
# via hickory (no mDNS), and by TLS fingerprint (sha256 of ~/.config/lan-mouse/
# lan-mouse.pem, which the daemon generates on first start).
#
# Firewall: UDP 4242 must be open on every listed host (done in each host's
# NixOS config, not here).
{
  lib,
  osConfig,
  inputs,
  ...
}:
let
  hostName = osConfig.networking.hostName or "";
  # Layout, left to right: game | gz-pc
  peers = {
    game = {
      ips = [ "192.168.64.245" ];
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
  others = lib.filterAttrs (n: _: n != hostName) peers;
in
{
  imports = [ inputs.xiongchenyu6.homeModules.lan-mouse ];

  services.lan-mouse = lib.mkIf (peers ? ${hostName}) {
    enable = true;
    settings = {
      port = 4242;
      authorized_fingerprints = lib.mapAttrs' (n: p: lib.nameValuePair p.fingerprint n) others;
      clients = lib.mapAttrsToList (n: p: {
        hostname = n;
        inherit (p) ips;
        position = layout.${hostName}.${n};
        activate_on_startup = true;
      }) others;
    };
  };
}
