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
    ezModules.root
    ezModules."freeman.xiong"
    ezModules.core
    ezModules.server
    ezModules.acme
    ezModules.datadog-agent
    ezModules.postgrest
    srvos.nixosModules.server
    srvos.nixosModules.hardware-amazon
    srvos.nixosModules.mixins-nginx
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
    robot_signal_dashboard.nixosModules.robotSignalDashboard
    rust-web-server.nixosModules.rust-web-server
    vscode-server.nixosModules.default
  ];

  users = {
    users = {
      root = {
        openssh = {
          authorizedKeys.keys = [
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHI7o9MTDU81RCkqhKbnXHqJgdhal7adqUZDhKUAWxLc server-benjamintan"
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBQBxWE+A+8sO3oszjEiNSAjsoEUfltBJMqpRRO3ieUp cardno:32_087_478"
            "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDc+zRGg+dTUqo0XAbCGuyGZiRlZwY0MdjOCrpZpcNDBAIk940/epjlkKh/2WTz4hC4hkv4Ms30cCbcbAByATNDXyVKwUrxkAJnFsXU4dgrYZJ0WR+67AeKRb+41daEFhmSQQztji5KmyF0uCMkBGNPC3jF63ybPj0UAS59g761t25P21IeS6zOf5IjBDIxp7JvtgOnIoOT4qQDTJijdCqjJJ/vP8bWmvS8mCMS5HDAKHgfEk5a3eJGFR8AngFSp1DkH5Q9y+YkM42IVrU2UkT8a4Qi/J2BCUKUCDBSeEmmgoBd8NOpkwvjcm6HY92sVZLSjeIyuZrEy9luNMC38PZ1SLNhDDiESIYiFpBWDKrOH8TkZStwpwwIb6hcm/+Tew5kYQlyAeCu2ZT97TRp7978cJP9Isz2kSdLRiLp57T4462feYEOvEJrBxultnCdYk6h/B9KJ81XDGG6Zt9i4A6jUBA26a7TEA6RAr8YLRKzUtr5BHpkYcAfVYTysDklU00= summer@summers-MacBook-Pro.local"
          ];
        };
      };
    };
  };

  sops.secrets."netbird/coturn/password" = {
    owner = "turnserver";
    group = "turnserver";
  };

  sops.secrets."rust-web-server/config" = { };

  environment = {
    systemPackages = [ pkgs.pgcli ];
  };

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
          4000
          4001
          4002
          5432
          7000
          7777
          6696
          33434
        ];
      };
    };

  security = {
    acme = {
      defaults = {
        group = "nginx";
      };
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
                href = "https://netbird.${config.networking.domain}";
              };
            }
            {
              "Frp" = {
                description = "Frp";
                href = "https://frp-dashboard.${config.networking.domain}";
              };
            }
          ];
        }
        {
          "Robot" = [
            {
              "Robot Dashboard" = {
                description = "Robot Dashboard";
                href = "https://robot-match.${config.networking.domain}";
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
    postgrest = {
      enable = true;
      pgpassFile = config.sops.secrets."postgrest/pass".path;
      settings = {
        db-uri = {
          host = "localhost";
          port = "5432";
          user = "rustwebserver";
          dbname = "rustWebServer";
        };
        db-anon-role = "rustwebserver";
        server-port = 3333; # use unix socket
        server-unix-socket = null;
        openapi-server-proxy-uri = "https://api.${config.networking.domain}";
        openapi-security-active = true;
      };
    };
    postgresqlBackup = {
      enable = true;
      databases = [ "rustWebServer" ];

    };

    postgresql = {
      enable = true;
      package = pkgs.postgresql_17_jit;
      authentication = ''
        local all all trust
        host  all  all 0.0.0.0/0 scram-sha-256
      '';

      enableJIT = true;
      enableTCPIP = true;
      extensions =
        ps: with ps; [
          pg_cron
        ];
      settings = {
        log_connections = true;
        log_statement = "all";
        logging_collector = true;
        log_disconnections = true;
        log_destination = lib.mkForce "syslog";
        shared_preload_libraries = "pg_cron";
        "cron.database_name" = "api";
        "cron.use_background_workers" = "on";
        max_worker_processes = 20;
      };
      ensureUsers = [
        {
          name = "freeman.xiong";
          ensureDBOwnership = true;
          ensureClauses = {
            superuser = true;
          };
        }
      ];
      ensureDatabases = [ "freeman.xiong" ];
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
      server = {
        domain = "netbird.${config.networking.domain}";
        management = {
          enable = true;
          domain = "netbird.${config.networking.domain}";
          enableNginx = true;
          oidcConfigEndpoint = "https://dev-bcz6ouy6jucjcnut.jp.auth0.com/.well-known/openid-configuration";
          turnDomain = "netbird.${config.networking.domain}";
          turnPort = 3478;
          metricsPort = 9092;
          settings = {
            TURNConfig = {
              Secret = "secret";
              Turns = [
                {
                  Proto = "udp";
                  URI = "turn:netbird.${config.networking.domain}:3478";
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
          domain = "netbird.${config.networking.domain}";
        };
        coturn = {
          enable = true;
          useAcmeCertificates = true;
          passwordFile = config.sops.secrets."netbird/coturn/password".path;
          domain = "netbird.${config.networking.domain}";
        };
        dashboard = {
          enable = true;
          domain = "netbird.${config.networking.domain}";
          enableNginx = true;
          managementServer = "https://netbird.${config.networking.domain}";
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
          useACMEHost = "${config.networking.domain}";
          kTLS = true;
        };
        "rust-server.${config.networking.domain}" = {
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "${config.networking.domain}";
          kTLS = true;
          locations = {
            "/" = {
              proxyPass = "http://localhost:3000";
            };
          };
        };
        "vr-sg.${config.networking.domain}" = {
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "${config.networking.domain}";
          kTLS = true;
          locations = {
            "/" = {
              proxyPass = "http://localhost:8080";
            };
          };
        };
        "frp-dashboard.${config.networking.domain}" = {
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "${config.networking.domain}";
          kTLS = true;
          locations = {
            "/" = {
              proxyPass = "http://localhost:7500";
            };
          };
        };
        "ollama.${config.networking.domain}" = {
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "${config.networking.domain}";
          kTLS = true;
          locations = {
            "/api/" = {
              proxyPass = "http://100.81.206.175:11434/api/";
            };
            "/" = {
              proxyPass = "http://100.81.206.175:3000";
            };
          };
        };
        "mngt.${config.networking.domain}" = {
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "${config.networking.domain}";
          kTLS = true;
          locations = {
            "/" = {
              proxyWebsockets = true;
              proxyPass = "http://localhost:5555";
            };
          };
        };
        "www.${config.networking.domain}" = {
          addSSL = true;
          acmeRoot = null;
          useACMEHost = "${config.networking.domain}";
          kTLS = true;
          locations = {
            "/" = {
              proxyWebsockets = true;
              proxyPass = "http://localhost:8082";
            };
          };
        };
        "${config.networking.domain}" = {
          addSSL = true;
          acmeRoot = null;
          useACMEHost = "${config.networking.domain}";
          kTLS = true;
          locations = {
            "/" = {
              proxyWebsockets = true;
              proxyPass = "http://localhost:8082";
            };
          };
        };
        "api.${config.networking.domain}" = {
          addSSL = true;
          acmeRoot = null;
          useACMEHost = "${config.networking.domain}";
          kTLS = true;
          locations = {
            "/" = {
              proxyWebsockets = true;
              proxyPass = "http://localhost:3333";
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
