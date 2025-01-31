{
  modulesPath,
  inputs,
  lib,
  ezModules,
  config,
  pkgs,
  shares,
  ...
}:
{
  imports = with inputs; [
    "${modulesPath}/installer/scan/not-detected.nix"
    "${modulesPath}/profiles/qemu-guest.nix"
    ./disk-config.nix
    ezModules.root
    ezModules."freeman.xiong"
    ezModules.core
    ezModules.server
    ezModules.sops
    ezModules.acme
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
    vscode-server.nixosModules.default
  ];

  sops.secrets."netbird/coturn/password" = {
    owner = "turnserver";
    group = "turnserver";
  };

  sops.secrets."rust-web-server/config" = { };

  zramSwap.enable = true;

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
          22
          80
          443
          2222
          5432
          7000
        ];
        allowedUDPPorts = [
          89
          179
          2222
          3478
          5432
          7000
          7777
          6696
          33434
        ];
      };
    };

  services = {
    homepage-dashboard = {
      enable = true;
      services = [
        {
          "Network" = [
            {
              "Netbird" = {
                description = "VPN";
                href = "https://netbird.autolife-robotics.me";
              };
            }
            {
              "Frp" = {
                description = "Frp";
                href = "https://frp-dashboard.autolife-robotics.me";
              };
            }
          ];
        }
        {
          "Robot" = [
            {
              "Robot Dashboard" = {
                description = "Robot Dashboard";
                href = "https://robot-match.autolife-robotics.me";
              };
            }
            {
              www = {
                description = "main page";
                href = "https://autolife.ai";
              };
            }
          ];
        }
      ];
      widgets = [
        {
          resources = {
            cpu = true;
            disk = "/";
            memory = true;
          };
        }
        {
          search = {
            provider = "duckduckgo";
            target = "_blank";
          };
        }
      ];
      bookmarks = [
        {
          Developer = [
            {
              Github = [
                {
                  abbr = "GH";
                  href = "https://github.com/AutoLifeRobot";
                }
              ];
            }
          ];
        }
        {
          Entertainment = [
            {
              Lark = [
                {
                  abbr = "LK";
                  href = "https://j403tw1dmh4.sg.larksuite.com/wiki";
                }
              ];
            }
          ];
        }
      ];
    };

    postgresql = {
      enable = true;
      package = pkgs.postgresql_17_jit;
      authentication = ''
        local all all trust
        host  all  all 0.0.0.0/0 scram-sha-256
      '';
      enableTCPIP = true;
      ensureDatabases = [ "freeman.xiong" ];
    };

    robotSignalDashboard = {
      enable = true;
      configFile = ./config.json;
    };

    rust-web-server = {
      enable = true;
      configFile = config.sops.secrets."rust-web-server/config".path;
    };

    frp = {
      enable = true;
      role = "server";
      settings = {
        bindPort = 7000;
        bindAddr = "0.0.0.0";
        kcpBindPort = 7000;
        vhostHTTPPort = 8080;
        webserver = {
          port = 7500;
          user = "admin";
          password = "admin";
        };
        auth = {
          method = "token";
          token = builtins.getEnv "FRP";
        };
      };
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
          turnDomain = "netbird.autolife-robotics.me";
          turnPort = 3478;
          metricsPort = 9092;
          settings = {
            TURNConfig = {
              Secret = "secret";
              Turns = [
                {
                  Proto = "udp";
                  URI = "turn:netbird.autolife-robotics.me:3478";
                  Username = "netbird";
                  Password = builtins.getEnv "TRUN_PASSWORD";
                }
              ];
            };
            HttpConfig = {
              AuthAudience = "https://dev-bcz6ouy6jucjcnut.jp.auth0.com/api/v2/";
              AuthIssuer = "https://dev-bcz6ouy6jucjcnut.jp.auth0.com/";
              AuthKeysLocation = "https://dev-bcz6ouy6jucjcnut.jp.auth0.com/.well-known/jwks.json";
              IdpSignKeyRefreshEnabled = false;
              AuthUserIDClaim = "";
              CertFile = "";
              CertKey = "";
            };
            DataStoreEncryptionKey = builtins.getEnv "DataStoreEncryptionKey";
            IdpManagerConfig = {
              ManagerType = "auth0";
              ClientConfig = {
                Issuer = "https://dev-bcz6ouy6jucjcnut.jp.auth0.com";
                TokenEndpoint = "https://dev-bcz6ouy6jucjcnut.jp.auth0.com/oauth/token";
                ClientID = "eD2cdw9iyfqEgZWWdCHeQ4xSeT30jYJW";
                ClientSecret = builtins.getEnv "IDP_ClientSecret";
              };
              ExtraConfig = {
                Audience = "https://dev-bcz6ouy6jucjcnut.jp.auth0.com/api/v2/";
              };
            };
            DeviceAuthorizationFlow = {
              Provider = "hosted";
              ProviderConfig = {
                Audience = "https://dev-bcz6ouy6jucjcnut.jp.auth0.com/api/v2/";
                Domain = null;
                AuthorizationEndpoint = "";
                ClientID = "EPjADNz97o2MjAEGdGd7cbxjM33PC8ZJ";
                TokenEndpoint = "https://dev-bcz6ouy6jucjcnut.jp.auth0.com/oauth/token";
                DeviceAuthEndpoint = "https://dev-bcz6ouy6jucjcnut.jp.auth0.com/oauth/device/code";
              };
            };
            PKCEAuthorizationFlow = {
              ProviderConfig = {
                Audience = "https://dev-bcz6ouy6jucjcnut.jp.auth0.com/api/v2/";
                ClientID = "QoD48IZw95dyYkn7ZMCCGIDVYwGZ94X3";
                AuthorizationEndpoint = "https://dev-bcz6ouy6jucjcnut.jp.auth0.com/authorize";
                TokenEndpoint = "https://dev-bcz6ouy6jucjcnut.jp.auth0.com/oauth/token";
                Domain = "";
              };
            };
          };
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
        "rust-server.autolife-robotics.me" = {
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "netbird.autolife-robotics.me";
          kTLS = true;
          locations = {
            "/" = {
              proxyPass = "http://localhost:3000";
            };
          };
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
        "frp-dashboard.autolife-robotics.me" = {
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "netbird.autolife-robotics.me";
          kTLS = true;
          locations = {
            "/" = {
              proxyPass = "http://localhost:7500";
            };
          };
        };
        "mngt.autolife-robotics.me" = {
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "netbird.autolife-robotics.me";
          kTLS = true;
          locations = {
            "/" = {
              proxyWebsockets = true;
              proxyPass = "http://localhost:5555";
            };
          };
        };
        "www.autolife-robotics.me" = {
          addSSL = true;
          acmeRoot = null;
          useACMEHost = "netbird.autolife-robotics.me";
          kTLS = true;
          locations = {
            "/" = {
              proxyWebsockets = true;
              proxyPass = "http://localhost:8082";
            };
          };
        };
        "autolife-robotics.me" = {
          addSSL = true;
          acmeRoot = null;
          useACMEHost = "netbird.autolife-robotics.me";
          kTLS = true;
          locations = {
            "/" = {
              proxyWebsockets = true;
              proxyPass = "http://localhost:8082";
            };
          };
        };
      };
    };
  };

  home-manager = {
    users = {
      "freeman.xiong" = {
        programs = {
          tmux.enable = true;
        };
      };
    };
  };
}
