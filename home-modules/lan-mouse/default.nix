# Software KVM between the workstations: one keyboard/mouse, switch by pushing
# the pointer across the screen edge. Module lives in NUR (lan-mouse has no
# nixpkgs/home-manager module; services.synergy is X11-only and cannot capture
# input under niri).
#
# Both sides run the same daemon; each host lists the other as a client on the
# matching side. Peers are pinned by IP because lan-mouse resolves hostnames
# via hickory (no mDNS), and by TLS fingerprint. The firewall is opened by
# nixos-modules/lan-mouse.nix from the same peer list.
{
  lib,
  osConfig,
  inputs,
  ...
}:
let
  inherit (import ./peers.nix) peers layout;
  hostName = osConfig.networking.hostName or "";
  others = lib.filterAttrs (n: _: n != hostName) peers;
in
{
  imports = [ inputs.xiongchenyu6.homeModules.lan-mouse ];

  services.lan-mouse = lib.mkIf (peers ? ${hostName}) {
    enable = true;
    settings = {
      port = osConfig.services.lan-mouse.port or 4242;
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
