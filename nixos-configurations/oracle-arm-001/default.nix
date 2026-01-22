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
    ../../nixos-modules/rust-web-server-config.nix # Sops configuration for rust-web-server
    ../../nixos-modules/samba.nix # Samba for corp Windows device management
    xiongchenyu6.nixosModules.falcon-sensor # CrowdStrike Falcon for endpoint security
    disko.nixosModules.disko
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")
    ezModules.root
    ezModules."freeman.xiong"
    ezModules.core
    ezModules.server
    ezModules.acme
    ezModules.datadog-agent
    ezModules.sing-box
    ezModules.postgrest
    ../../nixos-modules/gotrue-supabase.nix
    rust-web-server.nixosModules.rust-web-server
    srvos.nixosModules.server
    srvos.nixosModules.mixins-nginx
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
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
  # rust-web-server secrets are now handled by the module itself

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
    (map lib.lowPrio [
      pkgs.curl
      pkgs.gitMinimal
    ])
    ++ (with pkgs; [
      samba
      # osquery is handled by services.osquery module
    ])
    ++ lib.optionals (inputs ? xiongchenyu6) (
      with inputs.xiongchenyu6.packages.${pkgs.system};
      [
        # Add any additional corp management tools
      ]
    );
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
      databases = [ "rustwebserver" ];
    };

    postgresql = {
      enable = true;
      package = pkgs.postgresql_18_jit;
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
      ensureDatabases = [
        "freeman.xiong"
      ];
    };

    rust-web-server = {
      enable = true;
      configFile = config.sops.templates."rust-web-server-config".path;
      #      logLevel = "info"; # Options: error, warn, info, debug, trace
    };

    nginx = {
      virtualHosts = {
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
        "auth.${config.networking.domain}" = {
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "${config.networking.domain}";
          kTLS = true;
          locations = {
            "/" = {
              proxyPass = "http://localhost:8081";
              proxyWebsockets = true;
              extraConfig = ''
                # CORS headers for frontend integration
                add_header 'Access-Control-Allow-Origin' '*' always;
                add_header 'Access-Control-Allow-Methods' 'GET, POST, PUT, DELETE, OPTIONS' always;
                add_header 'Access-Control-Allow-Headers' 'Accept, Authorization, Cache-Control, Content-Type, DNT, If-Modified-Since, Keep-Alive, Origin, User-Agent, X-Requested-With, apikey' always;

                # Handle preflight requests
                if ($request_method = 'OPTIONS') {
                  add_header 'Access-Control-Allow-Origin' '*';
                  add_header 'Access-Control-Allow-Methods' 'GET, POST, PUT, DELETE, OPTIONS';
                  add_header 'Access-Control-Allow-Headers' 'Accept, Authorization, Cache-Control, Content-Type, DNT, If-Modified-Since, Keep-Alive, Origin, User-Agent, X-Requested-With, apikey';
                  add_header 'Access-Control-Max-Age' 1728000;
                  add_header 'Content-Type' 'text/plain charset=UTF-8';
                  add_header 'Content-Length' 0;
                  return 204;
                }
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

  # Enable Samba for Windows device management
  services.samba.settings = {
    global = {
      workgroup = lib.mkForce "AUTOLIFE";
      "server string" = lib.mkForce "AutoLife Corp Server";
      realm = "AUTOLIFE.AI";
    };
    corp-shared = {
      path = "/srv/samba/corp-shared";
      browseable = "yes";
      "read only" = "no";
      "guest ok" = "no";
      "create mask" = "0664";
      "directory mask" = "0775";
      comment = "AutoLife Corp Shared Drive";
    };
  };

  # Create additional directories
  systemd.tmpfiles.rules = [
    "d /srv/samba/corp-shared 0775 samba samba"
    "d /var/www/fleet 0755 nginx nginx"
    # osquery log directory handled by official module
  ];

  nixpkgs = {
    hostPlatform = "aarch64-linux";
  };
}
