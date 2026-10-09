{ lib, pkgs, ... }:
{
  services.rustdesk-server = {
    enable = true;
    package = pkgs.callPackage ../../packages/rustdesk-server-api/package.nix { };
    signal.enable = false;
    relay.extraArgs = [
      "--port"
      "443"
      "-k"
      # Guangzhou's public key; no private signing key is needed on a relay.
      "lhxyZ6yuHEmEvU5MMQ3xjt0j4NjRWRMWIc+O06obPtk="
    ];
  };
  # Use the existing cloud ingress on 443. The WebSocket port 445 stays closed.
  networking.firewall.allowedTCPPorts = [ 443 ];
  systemd.services.rustdesk-relay.serviceConfig = {
    # Binding 443 needs this capability in the host's network namespace.
    PrivateUsers = lib.mkForce false;
    AmbientCapabilities = [ "CAP_NET_BIND_SERVICE" ];
    CapabilityBoundingSet = [ "CAP_NET_BIND_SERVICE" ];
  };
}
