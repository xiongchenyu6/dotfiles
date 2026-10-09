{ lib, pkgs, ... }:
let
  rustdesk = pkgs.callPackage ../../packages/rustdesk-wayland.nix { };
  serverConfig = (pkgs.formats.toml { }).generate "RustDesk2.toml" {
    rendezvous_server = "rustdesk.gz.autolife.ai:7916";
    nat_type = 0;
    serial = 0;
    options = {
      custom-rendezvous-server = "rustdesk.gz.autolife.ai:7916";
      relay-server = "rustdesk.gz.autolife.ai:7917";
      api-server = "https://rustdesk.gz.autolife.ai:8444";
      key = "lhxyZ6yuHEmEvU5MMQ3xjt0j4NjRWRMWIc+O06obPtk=";
    };
  };
in
{
  environment.systemPackages = [ rustdesk ];
  boot.kernelModules = [ "uinput" ];
  networking.firewall.interfaces = {
    eno1.allowedTCPPorts = [ 21118 ];
    wlp4s0.allowedTCPPorts = [ 21118 ];
  };
  # RustDesk's GStreamer pipeline needs SHM buffers; niri's native Mutter
  # stream is DMA-BUF-only. The wlr bridge also offers SHM capture.
  xdg.portal.config.niri."org.freedesktop.impl.portal.ScreenCast" = lib.mkForce [ "wlr" ];
  # With PipeWire 1.6.9 the driving wlr stream sends one frame and then
  # receives no process callback. Explicitly trigger after returning a frame.
  nixpkgs.overlays = [
    (_: prev: {
      xdg-desktop-portal-wlr = prev.xdg-desktop-portal-wlr.overrideAttrs (old: {
        patches = (old.patches or [ ]) ++ [ ../../packages/xdg-desktop-portal-wlr-trigger.patch ];
      });
    })
  ];

  # Seed writable settings once; RustDesk owns passwords and login state.
  home-manager.users."freeman.xiong" = { lib, ... }: {
    home.activation.rustdeskConfig = lib.hm.dag.entryAfter [ "writeBoundary" ] ''
      configDir="$HOME/.config/rustdesk"
      run mkdir -p "$configDir"
      if [ ! -e "$configDir/RustDesk2.toml" ]; then
        run install -m 0600 ${serverConfig} "$configDir/RustDesk2.toml"
      fi
    '';
  };

  systemd.services.rustdesk = {
    description = "RustDesk remote desktop";
    # RustDesk starts the session's --server via sudo and uses awk in its
    # process cleanup. Neither is in systemd's default PATH.
    path = [
      "/run/wrappers"
      pkgs.gawk
      pkgs.getent
      pkgs.procps
      pkgs.util-linux
    ];
    wantedBy = [ "multi-user.target" ];
    wants = [ "network-online.target" ];
    after = [
      "network-online.target"
      "systemd-user-sessions.service"
    ];
    environment = {
      HOME = "/root";
      GST_PLUGIN_SYSTEM_PATH_1_0 = "${pkgs.gst_all_1.gstreamer}/lib/gstreamer-1.0:${pkgs.pipewire}/lib/gstreamer-1.0:${pkgs.gst_all_1.gst-plugins-base}/lib/gstreamer-1.0";
      PULSE_LATENCY_MSEC = "60";
      PIPEWIRE_LATENCY = "1024/48000";
    };
    preStart = ''
      install -d -m 0700 /root/.config/rustdesk
      if [ ! -e /root/.config/rustdesk/RustDesk2.toml ]; then
        install -m 0600 ${serverConfig} /root/.config/rustdesk/RustDesk2.toml
      fi
    '';
    # Apply through RustDesk's IPC to the active session's writable config.
    postStart = ''
      ${rustdesk}/bin/rustdesk --option direct-server Y
    '';
    serviceConfig = {
      ExecStart = "${rustdesk}/bin/rustdesk --service";
      # The privileged DRM loader expects the Debian private-library path.
      # Expose it only inside this service's mount namespace.
      BindReadOnlyPaths = [ "${rustdesk}/lib:/usr/lib/rustdesk" ];
      KillMode = "mixed";
      TimeoutStopSec = 30;
      Restart = "on-failure";
      LimitNOFILE = 100000;
    };
  };
}
