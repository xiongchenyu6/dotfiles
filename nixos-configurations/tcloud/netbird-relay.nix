# NetBird Cloud SG primary data-plane relay (rels://relay-cloud.sg.autolife.ai:443/relay).
# Reuses tcloud's existing coturn service for STUN on UDP :3478 (NB_ENABLE_STUN=false)
# to avoid port 3478 bind collision.
{
  config,
  lib,
  pkgs,
  ...
}:
{
  sops.secrets."netbird/relay_secret" = {
    sopsFile = ../../secrets/netbird-relay.yaml;
    restartUnits = [ "netbird-relay.service" ];
  };

  sops.templates."netbird-relay.env" = {
    owner = "root";
    group = "root";
    mode = "0400";
    restartUnits = [ "netbird-relay.service" ];
    content = ''
      NB_LOG_LEVEL=info
      NB_LOG_FILE=console
      NB_LISTEN_ADDRESS=127.0.0.1:33080
      NB_EXPOSED_ADDRESS=rels://relay-cloud.sg.autolife.ai:443/relay
      NB_AUTH_SECRET=${config.sops.placeholder."netbird/relay_secret"}
      NB_ENABLE_STUN=false
    '';
  };

  # netbird-relay only exposes a metrics port option, not a bind address.
  # Block external access to :9090 in prerouting while keeping loopback open.
  networking.nftables.tables.netbird-relay-metrics = {
    family = "inet";
    content = ''
      chain prerouting {
        type filter hook prerouting priority -310; policy accept;
        iifname != "lo" fib daddr type local tcp dport 9090 drop
      }
    '';
  };

  # Use HTTP-01 via nginx :80 so cert issuance for autolife.ai does not depend
  # on personal Cloudflare DNS-01 credentials.
  security.acme.certs."relay-cloud.sg.autolife.ai" = {
    dnsProvider = lib.mkForce null;
    environmentFile = lib.mkForce null;
  };

  services.nginx.virtualHosts."relay-cloud.sg.autolife.ai" = {
    enableACME = true;
    forceSSL = true;
    locations."/relay" = {
      proxyPass = "http://127.0.0.1:33080";
      proxyWebsockets = true;
      extraConfig = ''
        proxy_set_header X-Real-Port $remote_port;
        proxy_read_timeout 3600s;
        proxy_send_timeout 3600s;
        proxy_buffering off;
      '';
    };
    locations."/".return = "404";
  };

  systemd.services.netbird-relay = {
    description = "NetBird Cloud SG relay";
    wantedBy = [ "multi-user.target" ];
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    serviceConfig = {
      DynamicUser = true;
      EnvironmentFile = [ config.sops.templates."netbird-relay.env".path ];
      ExecStart = "${lib.getExe pkgs.netbird-relay} --health-listen-address=127.0.0.1:19090 --metrics-port=9090 --trusted-proxies=127.0.0.1/32,::1/128";
      Restart = "on-failure";
      RestartSec = 5;
      LimitNOFILE = 65536;
      NoNewPrivileges = true;
      PrivateTmp = true;
      ProtectHome = true;
      ProtectSystem = "strict";
      RestrictAddressFamilies = [
        "AF_INET"
        "AF_INET6"
        "AF_UNIX"
      ];
    };
  };
}
