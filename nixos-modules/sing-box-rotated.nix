{
  config,
  lib,
  pkgs,
  ...
}:
let
  secret = config.sops.secrets."sing-box-rotated/hysteria2-password";
  configTemplate = pkgs.writeText "sing-box-rotated.json" (
    builtins.toJSON {
      log.level = "info";
      inbounds = [
        {
          type = "hysteria2";
          tag = "hysteria2-rotated";
          listen = "::";
          listen_port = 8444;
          users = [ { password = ""; } ];
          tls = {
            enabled = true;
            certificate_path = "/var/lib/acme/panda.qzz.io/fullchain.pem";
            key_path = "/var/lib/acme/panda.qzz.io/key.pem";
          };
        }
      ];
      outbounds = [
        {
          type = "direct";
          tag = "direct";
        }
      ];
    }
  );
in
{
  # Ensure sing-box system user exists even on hosts that do not enable the
  # legacy services.sing-box (:8443) unit, and grant read access to the
  # acme:nginx panda.qzz.io certificate directory.
  users.groups.sing-box = { };
  users.users.sing-box = {
    isSystemUser = true;
    group = "sing-box";
    extraGroups = [ "nginx" ];
  };

  # Keep the existing 8443 listener and its clients alive during migration.
  # This separate credential must never restart sing-box.service.
  sops.secrets."sing-box-rotated/hysteria2-password" = {
    sopsFile = ../secrets/sing-box-rotation.yaml;
    key = "hysteria2_password";
    owner = "sing-box";
    mode = "0400";
    restartUnits = [ "sing-box-rotated.service" ];
  };

  boot.kernel.sysctl = {
    "net.core.default_qdisc" = lib.mkDefault "fq";
    "net.ipv4.tcp_congestion_control" = lib.mkDefault "bbr";
    "net.core.rmem_max" = lib.mkDefault 16777216;
    "net.core.wmem_max" = lib.mkDefault 16777216;
  };

  networking.firewall.allowedUDPPorts = [ 8444 ];

  # Hysteria2 UDP port hopping (30000-30999 -> :8444) defeats China Mobile's
  # per-5-tuple UDP throttling while keeping static :8444 reachable for probes.
  # Scoped to non-loopback local destinations so forwarded/container traffic
  # is never affected; post-redirect dport 8444 is permitted above.
  networking.nftables.tables.hy2-port-hop = {
    family = "inet";
    content = ''
      chain prerouting {
        type nat hook prerouting priority dstnat; policy accept;
        iifname != "lo" fib daddr type local udp dport 30000-30999 counter redirect to :8444
      }
    '';
  };

  systemd.services.sing-box-rotated = {
    description = "Hysteria2 listener for credential migration";
    wantedBy = [ "multi-user.target" ];
    after = [
      "network-online.target"
      "acme-finished-panda.qzz.io.target"
    ];
    wants = [ "network-online.target" ];
    preStart = ''
      ${pkgs.jq}/bin/jq --rawfile password ${secret.path} \
        '.inbounds[0].users[0].password = ($password | rtrimstr("\n"))' \
        ${configTemplate} > "$RUNTIME_DIRECTORY/config.json"
      ${pkgs.sing-box}/bin/sing-box check -c "$RUNTIME_DIRECTORY/config.json"
    '';
    serviceConfig = {
      User = "sing-box";
      Group = "sing-box";
      RuntimeDirectory = "sing-box-rotated";
      RuntimeDirectoryMode = "0700";
      StateDirectory = "sing-box-rotated";
      StateDirectoryMode = "0700";
      UMask = "0077";
      ExecStart = "${pkgs.sing-box}/bin/sing-box -D /var/lib/sing-box-rotated -c /run/sing-box-rotated/config.json run";
      Restart = "on-failure";
      RestartSec = "5s";
      LimitNOFILE = 65536;
      NoNewPrivileges = true;
      ProtectHome = true;
      PrivateTmp = true;
    };
  };
}
