{
  profiles,
  config,
  lib,
  pkgs,
  ...
}:
{

  imports = [
    ../../../users/root/nixos.nix
    ../../../users/freeman.xiong
    ../../../profiles/sops.nix
    ../../../profiles/core/nixos.nix
    ../../../profiles/server/components
    ../../../profiles/common/components
    ../../../profiles/common/components/datadog-agent.nix
    ./hardware-configuration.nix
    ../../../profiles/server/apps/proxy/nginx.nix
    ../../../profiles/server/apps/acme
    ./networking.nix
  ];
  sops.secrets."netbird/coturn/password" = {
    owner = "turnserver";
    group = "turnserver";
  };
  boot = {
    kernel = {
      sysctl = {
        "net.ipv4.ip_forward" = 1;
        "net.ipv4.conf.default.rp_filter" = 0;
        "net.ipv4.conf.all.rp_filter" = 0;
        "net.ipv4.conf.default.forwarding" = 1;
        "net.ipv4.conf.all.forwarding" = 1;
        "net.ipv6.conf.all.accept_redirects" = 0;
        "net.ipv6.conf.default.forwarding" = 1;
        "net.ipv6.conf.all.forwarding" = 1;
      };
    };
  };

  networking =
    let
      file-path = builtins.split "/" (toString ./.);
      hostName = lib.last file-path;
    in
    {
      inherit hostName;
      firewall = {
        allowedTCPPorts = [
          80
          443
          2222
        ];
        allowedUDPPorts = [
          89
          179
          2222
          3478
          6696
          33434
        ];
        enable = false;
      };
    };
  services = {
    openssh = {
      openFirewall = true;
    };
    frp = {
      enable = true;
      role = "server";
      settings = {
        bindPort = 7000;
        bindAddr = "0.0.0.0";
        kcpBindPort = 7000;
        vhostHTTPPort = 8080;
        auth = {
          method = "token";
          token = builtins.readFile ../../../secrets/frp.token;
        };
      };
    };
    do-agent = {
      enable = true;
    };
    netbird = {
      enable = true;
      server = {
        enable = true;
        enableNginx = true;
        domain = "netbird.autolife-robotics.me";
        management = {
          enable = true;
          domain = "netbird.autolife-robotics.me";
          enableNginx = true;
          oidcConfigEndpoint = "https://dev-bcz6ouy6jucjcnut.jp.auth0.com/.well-known/openid-configuration";
          settings = lib.importJSON ../../../secrets/management.password;
        };
        signal = {
          enable = true;
          enableNginx = true;
          domain = "netbird.autolife-robotics.me";
        };
        coturn = {
          enable = true;
          useAcmeCertificates = true;
          passwordFile = config.sops.secrets."netbird/coturn/password".path;
        };
        dashboard = {
          enable = true;
          domain = "netbird.autolife-robotics.me";
          managementServer = "https://netbird.autolife-robotics.me";
          settings = {
            AUTH_AUDIENCE = "https://dev-bcz6ouy6jucjcnut.jp.auth0.com/api/v2/";
            AUTH_CLIENT_ID = "QoD48IZw95dyYkn7ZMCCGIDVYwGZ94X3";
            AUTH_AUTHORITY = "https://dev-bcz6ouy6jucjcnut.jp.auth0.com/";
            USE_AUTH0 = "true";
            AUTH_SUPPORTED_SCOPES = "openid profile email offline_access api email_verified";
            NETBIRD_TOKEN_SOURCE = "accessToken";
          };
        };
      };
    };
    nginx = {
      virtualHosts = {
        "${config.services.netbird.server.domain}" = {
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "netbird.autolife-robotics.me";
          kTLS = true;
        };
        "vr-sg.autolife-robotics.me" = {
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "netbird.autolife-robotics.me";
          kTLS = true;
          locations = {
            "/" = {
              proxyPass = "http://localhost:8080";
            };
          };
        };
      };
    };
  };
}
