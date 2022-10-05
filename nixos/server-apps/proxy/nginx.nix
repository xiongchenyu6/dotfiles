{ config, pkgs, lib, symlinkJoin, domain, ... }:
let
  common-files-path = ../../common;
  secret-files-paht = common-files-path + "/secrets";
  script = import ../../dn42/update-roa.nix { inherit pkgs; };
  share = import (common-files-path + /share.nix);
in
{

  services = {
    nginx = {
      enable = true;
      statusPage = true;
      recommendedProxySettings = true;
      gitweb = { enable = true; };
      additionalModules = [ pkgs.nginxModules.pam ];

      virtualHosts = {
        bird-lg = {
          serverName = "bird-lg.inner.${domain}";
          addSSL = true;
          acmeRoot = null;
          useACMEHost = "inner.${domain}";
          kTLS = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:5000";
            proxyWebsockets = true;
          };
          extraConfig = ''
            auth_pam  "Password Required";
            auth_pam_service_name "nginx";
          '';
        };
        grafana = {
          serverName = "grafana.inner.${domain}";
          addSSL = true;
          acmeRoot = null;
          useACMEHost = "inner.${domain}";
          kTLS = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:${toString config.services.grafana.port}";
            proxyWebsockets = true;
          };
        };
        hydra = {
          serverName = "hydra.inner.${domain}";
          addSSL = true;
          acmeRoot = null;
          useACMEHost = "inner.${domain}";
          kTLS = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:3000";
            proxyWebsockets = true;
          };
        };
        prometheus = {
          serverName = "prometheus.inner.${domain}";
          addSSL = true;
          acmeRoot = null;
          useACMEHost = "inner.${domain}";
          kTLS = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:9090";
            proxyWebsockets = true;
          };
        };

      };
    };
  };

  systemd.services.nginx.serviceConfig = {
    SupplementaryGroups = [ "shadow" ];
    NoNewPrivileges = lib.mkForce false;
    PrivateDevices = lib.mkForce false;
    ProtectHostname = lib.mkForce false;
    ProtectKernelTunables = lib.mkForce false;
    ProtectKernelModules = lib.mkForce false;
    RestrictAddressFamilies = lib.mkForce [ ];
    LockPersonality = lib.mkForce false;
    MemoryDenyWriteExecute = lib.mkForce false;
    RestrictRealtime = lib.mkForce false;
    RestrictSUIDSGID = lib.mkForce false;
    SystemCallArchitectures = lib.mkForce "";
    ProtectClock = lib.mkForce false;
    ProtectKernelLogs = lib.mkForce false;
    RestrictNamespaces = lib.mkForce false;
    SystemCallFilter = lib.mkForce "";
  };
}