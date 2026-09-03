# System side of the lan-mouse software KVM: opens UDP 4242 on every host in
# the shared peer list. The daemon and layout live in home-modules/lan-mouse.
{ config, inputs, ... }:
let
  inherit (import ../home-modules/lan-mouse/peers.nix) peers;
in
{
  imports = [ inputs.xiongchenyu6.nixosModules.lan-mouse ];

  services.lan-mouse.enable = peers ? ${config.networking.hostName};
}
