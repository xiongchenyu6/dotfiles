# Cloud SG primary Network-V3 relay on oracle-arm-001 (FleetControl node:
# sg-cloud-external). Robots and headsets probe every relay in FleetControl's
# signed directory and pick the fastest live relay automatically.
# Public values must match control_private.relay_nodes:
#   ip 138.2.95.174, http_port 18081, udp_port_offset 0,
#   management_url https://v3-relay-cloud.sg.autolife.ai (via fleet-gz keepwarm).
{
  config,
  lib,
  pkgs,
  ...
}:
let
  relay = pkgs.callPackage ../../packages/autolife-network-v3-relay/package.nix { };
  serviceName = "autolife-network-v3-relay";
  trustProxyServiceName = "autolife-network-v3-trust-proxy";
  trustProxyPort = 18448;
  hk200Address = "200.222.239.101";
  credentials = "/run/credentials/${serviceName}.service";
  tokens = "/etc/autolife/network-v3-relay";
  stateDirectory = "/var/lib/${serviceName}";
  managementPort = 18081;
  fleetHost = "fleet.autolife-robotics.com";
  fleetAddress = "42.193.160.128";
  streamPorts = base: {
    data = base;
    audio = base + 1;
    point_cloud = base + 2;
    video0 = base + 3;
    video1 = base + 4;
    video2 = base + 5;
  };
  ports = {
    wifi_probe = 5909;
    cellular_probe = 5919;
    lan_probe = 5929;
    http = managementPort;
    wifi = streamPorts 5901;
    cellular = streamPorts 5911;
    lan = streamPorts 5921;
    controller = streamPorts 5951;
  };
  relayConfig = (pkgs.formats.toml { }).generate "network-v3-relay.toml" {
    bind_ip = "0.0.0.0";
    advertised_ip = "138.2.95.174";
    endpoint_timeout_ms = 10000;
    max_robots = 1000;
    max_controllers = 1000;
    max_watchers_per_robot = 8;
    udp_receive_buffer_bytes = 4194304;
    udp_send_buffer_bytes = 4194304;
    inherit ports;
    management = {
      bind_ip = "127.0.0.1";
      allow_unauthenticated = false;
      bearer_token_file = "${credentials}/relay-management.token";
      revoke_token_file = "${credentials}/relay-revoke.token";
    };
    v3_security = {
      max_ticket_lifetime_ms = 60000;
      allow_test_issuers = false;
      allow_control_ticket_reuse = false;
      trust_bundle_url = "https://${fleetHost}/api/rpc/v3_trust_bundle";
      trust_bundle_cache = "${stateDirectory}/trust-bundle.json";
      trust_bundle_refresh_ms = 30000;
      issuer_keys = [
        {
          key_id = 1;
          public_key_hex = "7b47a6b3e3f4db5a14adc5604bd201e302ae4c440bec520825f10b59e3aaf299";
        }
      ];
    };
  };
  checkCredentials = pkgs.writeScript "check-network-v3-relay-credentials" ''
    #!${pkgs.python3}/bin/python3
    import pathlib
    import sys
    directory = pathlib.Path("${credentials}")
    values = [(directory / name).read_text().strip() for name in (
        "relay-management.token", "relay-revoke.token")]
    if any(len(value.encode()) < 32 or any(c.isspace() for c in value) for value in values):
        sys.exit("Relay credentials must have at least 32 non-whitespace bytes")
    if values[0] == values[1]:
        sys.exit("Relay management and revoke credentials must be different")
  '';
in
{
  environment.etc."autolife/network-v3-relay.toml".source = relayConfig;

  networking.firewall.allowedUDPPorts = [
    ports.wifi_probe
    ports.cellular_probe
    ports.lan_probe
  ]
  ++ lib.concatMap (channel: builtins.attrValues ports.${channel}) [
    "wifi"
    "cellular"
    "lan"
    "controller"
  ];

  # Use HTTP-01 via nginx :80 so cert issuance for autolife.ai does not depend
  # on personal Cloudflare DNS-01 credentials.
  security.acme.certs."v3-relay-cloud.sg.autolife.ai" = {
    dnsProvider = lib.mkForce null;
    environmentFile = lib.mkForce null;
  };

  # FleetControl manages this relay over HTTPS :443; relay HTTP stays on loopback
  # and only FleetControl's egress IP is admitted.
  services.nginx.virtualHosts."v3-relay-cloud.sg.autolife.ai" = {
    enableACME = true;
    forceSSL = true;
    locations."/" = {
      proxyPass = "http://127.0.0.1:${toString managementPort}";
      extraConfig = ''
        allow ${fleetAddress}/32;
        deny all;
        client_max_body_size 16k;
        proxy_connect_timeout 2s;
        proxy_read_timeout 10s;
      '';
    };
  };

  # Loopback-only HTTP CONNECT proxy strictly scoped to FleetControl's trust
  # bundle endpoint via hk-200 (45ms + 9ms, 0% loss) to bypass direct SG->GZ
  # 30% packet loss. Every other destination/port is hard-rejected so zero
  # other host traffic (especially sing-box-rotated) can touch hk-200 quota.
  sops = {
    secrets = {
      "hk-200-trust-proxy/vless_uuid" = {
        sopsFile = ../../secrets/hk-200-trust-proxy.yaml;
        key = "vless_uuid";
        restartUnits = [ "${trustProxyServiceName}.service" ];
      };
      "hk-200-trust-proxy/reality_public_key" = {
        sopsFile = ../../secrets/hk-200-trust-proxy.yaml;
        key = "reality_public_key";
        restartUnits = [ "${trustProxyServiceName}.service" ];
      };
      "hk-200-trust-proxy/reality_short_id" = {
        sopsFile = ../../secrets/hk-200-trust-proxy.yaml;
        key = "reality_short_id";
        restartUnits = [ "${trustProxyServiceName}.service" ];
      };
    };

    templates."autolife-network-v3-trust-proxy.json" = {
      owner = "root";
      group = "root";
      mode = "0400";
      restartUnits = [ "${trustProxyServiceName}.service" ];
      content = builtins.toJSON {
        log.level = "warn";
        inbounds = [
          {
            type = "http";
            tag = "trust-proxy-in";
            listen = "127.0.0.1";
            listen_port = trustProxyPort;
          }
        ];
        outbounds = [
          {
            type = "vless";
            tag = "hk-200-vless";
            server = hk200Address;
            server_port = 443;
            uuid = config.sops.placeholder."hk-200-trust-proxy/vless_uuid";
            flow = "xtls-rprx-vision";
            tls = {
              enabled = true;
              server_name = "www.microsoft.com";
              utls = {
                enabled = true;
                fingerprint = "chrome";
              };
              reality = {
                enabled = true;
                public_key = config.sops.placeholder."hk-200-trust-proxy/reality_public_key";
                short_id = config.sops.placeholder."hk-200-trust-proxy/reality_short_id";
              };
            };
          }
        ];
        route = {
          rules = [
            {
              inbound = [ "trust-proxy-in" ];
              network = "tcp";
              port = [ 443 ];
              domain = [ fleetHost ];
              action = "route";
              outbound = "hk-200-vless";
            }
            {
              inbound = [ "trust-proxy-in" ];
              network = "tcp";
              port = [ 443 ];
              ip_cidr = [ "${fleetAddress}/32" ];
              action = "route";
              outbound = "hk-200-vless";
            }
            {
              action = "reject";
            }
          ];
        };
      };
    };
  };

  systemd = {
    tmpfiles.rules = [ "d ${tokens} 0700 root root -" ];

    services = {
      ${trustProxyServiceName} = {
        description = "Scoped loopback proxy for AutoLife Network V3 trust bundle via hk-200";
        wantedBy = [ "multi-user.target" ];
        after = [ "network-online.target" ];
        wants = [ "network-online.target" ];
        serviceConfig = {
          Type = "simple";
          DynamicUser = true;
          RuntimeDirectory = trustProxyServiceName;
          RuntimeDirectoryMode = "0700";
          LoadCredential = [
            "config.json:${config.sops.templates."autolife-network-v3-trust-proxy.json".path}"
          ];
          ExecStartPre = "${pkgs.sing-box}/bin/sing-box check -c %d/config.json";
          ExecStart = "${pkgs.sing-box}/bin/sing-box -D /run/${trustProxyServiceName} -c %d/config.json run";
          Restart = "on-failure";
          RestartSec = 3;
          UMask = "0077";
          LimitNOFILE = 4096;
          LimitCORE = 0;
          NoNewPrivileges = true;
          PrivateTmp = true;
          ProtectSystem = "strict";
          ProtectHome = true;
          RestrictAddressFamilies = [
            "AF_INET"
            "AF_INET6"
            "AF_UNIX"
          ];
        };
      };

      ${serviceName} = {
        description = "AutoLife Network V3 Cloud Singapore UDP relay";
        wantedBy = [ "multi-user.target" ];
        after = [
          "network-online.target"
          "time-sync.target"
          "${trustProxyServiceName}.service"
        ];
        wants = [
          "network-online.target"
          "time-sync.target"
          "${trustProxyServiceName}.service"
        ];
        # Tokens come from FleetControl (relay-tokens/sg-cloud-external*); until
        # installed, skip starting the service instead of failing activation.
        unitConfig.ConditionPathExists = [
          "${tokens}/relay-management.token"
          "${tokens}/relay-revoke.token"
        ];
        environment = {
          RUST_LOG = "info";
          AUTOLIFE_LOG_FORMAT = "json";
          SSL_CERT_FILE = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
          HTTPS_PROXY = "http://127.0.0.1:${toString trustProxyPort}";
          NO_PROXY = "127.0.0.1,localhost,::1";
        };
        serviceConfig = {
          Type = "simple";
          DynamicUser = true;
          StateDirectory = serviceName;
          StateDirectoryMode = "0700";
          LoadCredential = [
            "relay-management.token:${tokens}/relay-management.token"
            "relay-revoke.token:${tokens}/relay-revoke.token"
          ];
          ExecStartPre = [
            "${relay}/bin/autolife-relay-server --config ${relayConfig} --check-config"
            "${checkCredentials}"
          ];
          ExecStart = "${relay}/bin/autolife-relay-server --config ${relayConfig}";
          Restart = "on-failure";
          RestartSec = 3;
          Nice = -15;
          CPUWeight = 1000;
          IOSchedulingClass = "best-effort";
          IOSchedulingPriority = 0;
          OOMScoreAdjust = -500;
          UMask = "0077";
          LimitNOFILE = 131072;
          LimitCORE = 0;
          NoNewPrivileges = true;
          PrivateTmp = true;
          ProtectSystem = "strict";
          ProtectHome = true;
          RestrictAddressFamilies = [
            "AF_INET"
            "AF_INET6"
            "AF_UNIX"
          ];
        };
      };
    };
  };
}
