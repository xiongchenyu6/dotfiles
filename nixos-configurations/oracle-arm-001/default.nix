{
  inputs,
  modulesPath,
  lib,
  pkgs,
  ezModules,
  config,
  ...
}:
{
  imports = with inputs; [
    disko.nixosModules.disko
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")
    ezModules.root
    ezModules."freeman.xiong"
    ezModules.core
    ezModules.server
    ezModules.acme
    ezModules.datadog-agent
    ezModules.postgrest
    autolife-relay.nixosModules.autolife-relay
    srvos.nixosModules.server
    ezModules.mixins-nginx
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
    inputs.rust-web-server.nixosModules.rust-web-server
    ../../nixos-modules/rust-web-server-config.nix
    ./disk-config.nix
    ./hardware-configuration.nix
  ];
  boot = {
    loader.grub = {
      # no need to set devices, disko will add all devices that have a EF02 partition to the list already
      # devices = [ ];
      efiSupport = true;
      efiInstallAsRemovable = true;
    };
    kernel = {
      sysctl = {
        "net.ipv4.ip_forward" = 1;
      };
    };
  };

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

  environment.systemPackages =
    map lib.lowPrio [
      pkgs.curl
      pkgs.gitMinimal
    ]
    ++ (with pkgs; [
      nodejs_25
      osquery # is handled by services.osquery module
    ]);

  networking =
    let
      file-path = builtins.split "/" (toString ./.);
      hostName = lib.last file-path;
    in
    {
      inherit hostName;
      domain = "autolife.ai"; # Set corp domain
      firewall = {
        allowedTCPPorts = [
          22
          80
          443
          139 # NetBIOS Session Service
          445 # SMB
          2222
          5432
          7000
          8080 # Fleet
        ];
        allowedUDPPorts = [
          89
          137 # NetBIOS Name Service
          138 # NetBIOS Datagram Service
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
    postgresqlBackup = {
      enable = true;
      databases = [
        "rustwebserver"
        "odoo"
      ];
    };

    postgresql = {
      enable = true;
      package = pkgs.postgresql_18_jit;
      authentication = ''
        local all all trust
        host  all  all 0.0.0.0/0 scram-sha-256
        host  all  all ::1/128 scram-sha-256
        host odoo odoo 127.0.0.1/32 md5
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
        {
          name = "odoo";
          ensureDBOwnership = true;
        }
      ];
      ensureDatabases = [
        "freeman.xiong"
        "odoo"
      ];
    };

    autolife-relay = {
      enable = true;
      openFirewall = true;
      settings = {
        server_url = "ws://183.6.107.47:3000/ws";
        token = "@token@";
        region = "sg-1";
        ip = "138.2.95.174";
        bind_ip = "[::]";
        video_port = 30001;
        data_port = 30002;
        audio_port = 30003;
        probe_port = 30004;
        video_port_workers = 1;
        data_port_workers = 1;
        audio_port_workers = 1;
        telemetry_interval = 10;
        debug_stats_interval = 10;
        debug_stats_enabled = true;
        service_auth = {
          client = "autolife-relay";
          secret = "@service-auth-secret@";
        };
        license = {
          license_file = config.sops.secrets."autolife-relay/license".path;
          public_key = builtins.readFile ./id_ed25519.pub;
        };
      };
      sopsSecretFiles = [
        config.sops.secrets."autolife-relay/token".path
        config.sops.secrets."autolife-relay/service-auth-secret".path
      ];
    };

    # Odoo ERP/CRM system
    odoo = {
      enable = true;
      domain = "odoo.autolife.ai";
      autoInit = true;
      settings = {
        options = {
          # Database configuration — use unix socket (no password needed with peer/trust auth)
          db_host = "False";
          db_port = "5432";
          db_user = "odoo";
          db_maxconn = "64";

          # Server configuration
          list_db = "false";
          proxy_mode = lib.mkForce true;
          #workers = "4";
          max_cron_threads = "2";
          limit_request = "8192";
          limit_time_cpu = "600";
          limit_time_real = "1200";

          # File storage
          data_dir = lib.mkForce "/var/lib/odoo";
          logfile = "/var/log/odoo/odoo.log";
          log_level = "info";
          log_handler = "[':INFO']";

          # Security settings — admin_passwd managed by sops
          without_demo = "true";

          # Performance tuning
          osv_memory_age_limit = "1.0";
          osv_memory_count_limit = "0";
        };
      };
    };

    # Rust web server
    rust-web-server = {
      enable = true;
      configFile = config.sops.templates."rust-web-server-config".path;
      licenseFile = config.sops.templates."rust-web-server-license".path;
      publicKey = builtins.readFile ./id_ed25519.pub;
    };

    nginx = {
      commonHttpConfig = ''
        map $http_origin $cors_origin {
          default "";
          "~^https://.*\.autolife\.ai$" $http_origin;
          "https://autolife.ai" $http_origin;
        }
      '';
      virtualHosts = {
        "odoo.autolife.ai" = {
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "ai";
          kTLS = true;
        };
        "rust-server.autolife.ai" = {
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "ai";
          kTLS = true;
          locations = {
            "/" = {
              proxyPass = "http://localhost:3000";
            };
          };
        };
        "api.autolife.ai" = {
          addSSL = true;
          acmeRoot = null;
          useACMEHost = "ai";
          kTLS = true;
          locations = {
            "/" = {
              proxyWebsockets = true;
              proxyPass = "http://localhost:3333";
            };
          };
        };
        "auth.autolife.ai" = {
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "ai";
          kTLS = true;
          locations = {
            "/" = {
              proxyWebsockets = true;
              proxyPass = "http://127.0.0.1:8081";
              extraConfig = ''
                # CORS headers for auth endpoints — restrict to *.autolife.ai
                add_header 'Access-Control-Allow-Origin' $cors_origin always;
                add_header 'Access-Control-Allow-Methods' 'GET, POST, PUT, DELETE, OPTIONS' always;
                add_header 'Access-Control-Allow-Headers' 'Authorization, Content-Type, X-Client-Info, apikey' always;
                add_header 'Access-Control-Allow-Credentials' 'true' always;

                # Handle preflight requests
                if ($request_method = 'OPTIONS') {
                  return 204;
                }
              '';
            };
          };
        };
        "fleet.${config.networking.domain}" = {
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "${config.networking.domain}";
          kTLS = true;
          locations = {
            "/" = {
              root = "/var/www/fleet";
              index = "index.html";
              extraConfig = ''
                # Placeholder for Fleet UI - would host a web interface for device management
                try_files $uri $uri/ =404;
              '';
            };
          };
        };
      };
    };
  };

  # Enable osquery for device monitoring using official NixOS module
  services.osquery = {
    enable = true;
    settings = {
      options = {
        host_identifier = "hostname";
        schedule_splay_percent = 10;
        schedule_timeout = 0;
        schedule_max_drift = 60;
        logger_plugin = "filesystem";
        logger_path = "/var/log/osquery";
      };
      schedule = {
        system_info = {
          query = "SELECT hostname, cpu_brand, physical_memory FROM system_info;";
          interval = 3600;
        };
        processes = {
          query = "SELECT name, path, pid FROM processes;";
          interval = 600;
        };
      };
    };
  };

  # Log rotation for Odoo logs
  services.logrotate.settings.odoo = {
    files = [ "/var/log/odoo/*.log" ];
    frequency = "weekly";
    rotate = 4;
    compress = true;
    delaycompress = true;
    missingok = true;
    notifempty = true;
    create = "644 odoo odoo";
    postrotate = "systemctl reload odoo.service";
  };

  nixpkgs = {
    hostPlatform = "aarch64-linux";
    config.permittedInsecurePackages = [
      "python3.12-pypdf2-3.0.1"
    ];
    # overlays = [
    #   (final: prev: {
    #     odoo = prev.odoo.overrideAttrs (old: {
    #       patches = (old.patches or []) ++ [ ./patches/odoo-update.patch ];
    #     });
    #   })
    # ];
  };

  # Sops secrets for autolife-relay
  sops.templates."rust-web-server-license" = {
    content = config.sops.placeholder."autolife-relay/license";
    owner = "rust-web-server";
    group = "rust-web-server";
    mode = "0400";
  };
  sops.secrets."autolife-relay/license" = {
    owner = "autolife-relay";
    group = "autolife-relay";
  };
  sops.secrets."autolife-relay/token" = {
    owner = "autolife-relay";
    group = "autolife-relay";
  };
  sops.secrets."autolife-relay/service-auth-secret" = {
    owner = "autolife-relay";
    group = "autolife-relay";
  };

  # Sops secrets for Odoo
  sops.secrets."odoo/db_password" = {
    owner = "odoo";
    group = "odoo";
  };
  sops.secrets."odoo/admin_password" = {
    owner = "odoo";
    group = "odoo";
  };

}
