{ config, ... }:
# Headless Interactive Brokers Gateway (PAPER) for the quant US-equity engine.
#
# Why here: IB Gateway is an x86_64 GUI Java app with no real headless mode; it must
# run under a virtual display with IBC driving the login. oracle-arm-002 (aarch64) can't
# run it without emulation, so this x86_64 box is the sidecar. nautilus_equity on the mesh
# (download_ib.py now, the live equity node later) connects over wg0 to 172.22.240.97:4002.
#
# Implementation: the gnzsnz/ib-gateway image bundles Gateway + IBC + Xvfb + VNC and tracks
# IBKR's installer/version churn, which a hand-rolled buildFHSEnv derivation cannot. Still
# fully declarative + sops; one block to remove.
#
# PREREQUISITE — disable 2FA on the paper login (IBKR portal → Settings → User Settings →
# Secure Login System → exclude the paper user) or every daily restart blocks forever on a
# phone prompt. Paper accounts permit this; live never would.
let
  # Bind the API to the WireGuard mesh address ONLY — never the public interface.
  # wg0 is a trustedInterface on this host, so no firewall port needs opening.
  wgAddr = "172.22.240.97";
in
{
  # IB paper credentials. Add the values (never plaintext in the repo) with:
  #   sops secrets/common.yaml
  # then under the top level:
  #   ib-gateway:
  #     username: <paper user>
  #     password: <paper password>
  sops.secrets."ib-gateway/username" = { };
  sops.secrets."ib-gateway/password" = { };

  sops.templates."ib-gateway.env".content = ''
    TWS_USERID=${config.sops.placeholder."ib-gateway/username"}
    TWS_PASSWORD=${config.sops.placeholder."ib-gateway/password"}
  '';

  virtualisation.podman = {
    enable = true;
    autoPrune.enable = true;
  };

  virtualisation.oci-containers = {
    backend = "podman";
    containers.ib-gateway = {
      # :stable tracks IBKR Gateway bumps. Pin a digest (image = "...@sha256:...")
      # once a known-good build is soaked, so a silent upstream push can't change it.
      image = "ghcr.io/gnzsnz/ib-gateway:stable";
      environmentFiles = [ config.sops.templates."ib-gateway.env".path ];
      environment = {
        TRADING_MODE = "paper"; # SAFETY: paper only. live would be 4001 — intentionally unused.
        READ_ONLY_API = "yes"; # data + reconcile only. Flip to "no" when paper-TRADING (Stage 4).
        TWOFA_TIMEOUT_ACTION = "restart";
        AUTO_RESTART_TIME = "11:59 PM"; # daily Gateway self-restart, outside US regular hours
        TIME_ZONE = "America/New_York";
      };
      # Tune for the 956MB box: lower Gateway heap (768m->512m) so it stops swap-thrashing,
      # and raise IBC's login-dialog timeout (60->180s) so the slow GUI renders before IBC
      # gives up (the exit-1112 loop). Mounted over the image's files; the .vmoptions path
      # pins Gateway version 10.45.1g — revisit on a major Gateway bump.
      volumes = [
        "${./ibgateway.vmoptions}:/home/ibgateway/Jts/ibgateway/10.45.1g/ibgateway.vmoptions:ro"
        "${./ibc-config.ini.tmpl}:/home/ibgateway/ibc/config.ini.tmpl:ro"
      ];
      # Publish the paper API on the mesh IP only. The gnzsnz image binds Gateway's API to
      # 127.0.0.1:4002 INSIDE the container (localhost-trusted) and runs a socat relay on
      # container port 4004 -> 127.0.0.1:4002 so external clients appear as localhost. So we
      # map host 4002 -> container 4004 (the relay), NOT container 4002 (loopback-only).
      ports = [ "${wgAddr}:4002:4004" ];
      # First-run debugging over VNC (mesh-only). Uncomment + set VNC_SERVER_PASSWORD in
      # the sops env, watch the login succeed, then re-comment.
      # ports = [ "${wgAddr}:4002:4002" "${wgAddr}:5900:5900" ];
      extraOptions = [ "--pull=newer" ];
    };
  };
}
