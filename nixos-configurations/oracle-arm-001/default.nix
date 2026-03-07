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
      samba
      nodejs_22
      # osquery is handled by services.osquery module
    ])
    ++ lib.optionals (inputs ? xiongchenyu6) (
      with inputs.xiongchenyu6.packages.${pkgs.stdenv.hostPlatform.system};
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
      serverUrl = "ws://183.6.107.47:3000/ws";
      token = "SOPS_PLACEHOLDER_TOKEN";
      region = "sg-1";
      ip = "138.2.95.174";
      bindIp = "0.0.0.0";
      videPort = 30001;
      dataPort = 30002;
      audioPort = 30003;
      probePort = 30004;
      videoPortWorkers = 1;
      dataPortWorkers = 1;
      audioPortWorkers = 1;
      telemetryInterval = 10;
      debugStatsInterval = 10;
      debugStatsEnabled = true;
      serviceAuth = {
        client = "autolife-relay";
        secret = "SOPS_PLACEHOLDER_SERVICE_AUTH_SECRET";
      };
      license = {
        licenseFile = config.sops.secrets."autolife-relay/license".path;
        publicKey = builtins.readFile ./id_ed25519.pub;
      };
    };

    # Odoo ERP/CRM system
    odoo = {
      enable = true;
      package = pkgs.odoo.overridePythonAttrs (old: {
        # Fix worker spawning: Odoo workers re-exec via sys.argv[0].
        # The makeWrapperArgs creates a bash wrapper, but sys.argv[0] in the Python
        # wrapper points to it. When workers spawn, Python interprets bash as Python.
        # Solution: Remove makeWrapperArgs (no bash wrapper), keep Python wrapper
        # for PYTHONPATH. Add PATH deps via systemd instead.
        makeWrapperArgs = [ ];
      });
      domain = "odoo.autolife.ai";
      autoInit = true;
      settings = {
        options = {
          # Database configuration
          db_host = "localhost";
          db_port = "5432";
          db_user = "odoo";
          # db_password managed by sops — injected at runtime
          db_maxconn = "64";

          # Server configuration
          list_db = "false";
          proxy_mode = lib.mkForce true;
          workers = "4";
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

    nginx = {
      commonHttpConfig = ''
        map $http_origin $cors_origin {
          default "";
          "~^https://.*\.autolife\.ai$" $http_origin;
          "https://autolife.ai" $http_origin;
        }
      '';
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
    "d /var/lib/odoo 0755 odoo odoo -"
    "d /var/log/odoo 0755 odoo odoo -"
    # osquery log directory handled by official module
  ];

  # Inject autolife-relay secrets into config at runtime
  systemd.services.autolife-relay.serviceConfig.ExecStartPre = lib.mkBefore [
    "+${pkgs.writeShellScript "autolife-relay-inject-secrets" ''
      cfg="/var/lib/autolife-relay/config.yaml"
      # Copy the nix-store config to a writable location
      cp --no-preserve=mode $(grep -oP '(?<=--config-file )\S+' /etc/systemd/system/autolife-relay.service) "$cfg"
      # Inject sops secrets
      token=$(cat ${config.sops.secrets."autolife-relay/token".path})
      secret=$(cat ${config.sops.secrets."autolife-relay/service-auth-secret".path})
      ${pkgs.gnused}/bin/sed -i "s|SOPS_PLACEHOLDER_TOKEN|$token|g" "$cfg"
      ${pkgs.gnused}/bin/sed -i "s|SOPS_PLACEHOLDER_SERVICE_AUTH_SECRET|$secret|g" "$cfg"
      chown autolife-relay:autolife-relay "$cfg"
      chmod 0600 "$cfg"
    ''}"
  ];
  systemd.services.autolife-relay.serviceConfig.ExecStart =
    lib.mkForce "${config.services.autolife-relay.package}/bin/autolife-relay --config-file /var/lib/autolife-relay/config.yaml";

  # Add wkhtmltopdf and rtlcss to odoo service PATH (since we disabled wrapping)
  systemd.services.odoo.path = [
    pkgs.wkhtmltopdf
    pkgs.rtlcss
  ];

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
  };

  # Sops secrets for autolife-relay
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

  # Inject Odoo secrets into config at runtime
  systemd.services.odoo.serviceConfig.ExecStartPre = lib.mkAfter [
    "+${pkgs.writeShellScript "odoo-inject-secrets" ''
      cfg="/etc/odoo/odoo.cfg"
      db_pass=$(cat ${config.sops.secrets."odoo/db_password".path})
      admin_pass=$(cat ${config.sops.secrets."odoo/admin_password".path})
      ${pkgs.gnused}/bin/sed -i '/^db_password\s*=/d; /^admin_passwd\s*=/d' "$cfg"
      echo "db_password = $db_pass" >> "$cfg"
      echo "admin_passwd = $admin_pass" >> "$cfg"
    ''}"
  ];

  # Set Odoo database user password after PostgreSQL starts
  systemd.services.odoo-db-init = {
    description = "Initialize Odoo database user password";
    wantedBy = [ "odoo.service" ];
    before = [ "odoo.service" ];
    after = [
      "postgresql.service"
      "sops-nix.service"
    ];
    wants = [ "postgresql.service" ];
    script = ''
      # Wait for PostgreSQL to be ready
      while ! ${pkgs.postgresql_18_jit}/bin/psql -U freeman.xiong -d freeman.xiong -c '\q' 2>/dev/null; do
        echo "Waiting for PostgreSQL..."
        sleep 2
      done

      # Set odoo user password from sops secret
      DB_PASS=$(cat ${config.sops.secrets."odoo/db_password".path})
      echo "Setting odoo user password..."
      ${pkgs.postgresql_18_jit}/bin/psql -U freeman.xiong -d freeman.xiong -c "ALTER USER odoo PASSWORD '$DB_PASS';"
      echo "Odoo database password set successfully!"
    '';

    serviceConfig = {
      Type = "oneshot";
      User = "root";
    };
  };

  # OpenClaw — personal AI assistant
  users.users.openclaw = {
    isSystemUser = true;
    group = "openclaw";
    home = "/var/lib/openclaw";
    createHome = true;
  };
  users.groups.openclaw = { };

  systemd.services.openclaw = {
    description = "OpenClaw Gateway Service";
    after = [
      "network-online.target"
      "sops-nix.service"
    ];
    wants = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];

    path = with pkgs; [
      nodejs_22
      git
      bash
      coreutils
      gnused
      gnutar
      gzip
      findutils
      gnugrep
    ];

    # Install openclaw globally on first start or update
    preStart = ''
      export HOME=/var/lib/openclaw
      export NPM_CONFIG_PREFIX=/var/lib/openclaw/.npm-global
      export PATH="$NPM_CONFIG_PREFIX/bin:$PATH"
      if [ ! -x "$NPM_CONFIG_PREFIX/bin/openclaw" ]; then
        echo "Installing openclaw..."
        ${pkgs.nodejs_22}/bin/npm install -g openclaw@latest --prefix "$NPM_CONFIG_PREFIX" --ignore-scripts
      fi
    '';

    script = ''
      export HOME=/var/lib/openclaw
      export NPM_CONFIG_PREFIX=/var/lib/openclaw/.npm-global
      export PATH="$NPM_CONFIG_PREFIX/bin:$PATH"
      # Create minimal config if none exists
      mkdir -p /var/lib/openclaw/.openclaw
      if [ ! -f /var/lib/openclaw/.openclaw/openclaw.json ]; then
        cat > /var/lib/openclaw/.openclaw/openclaw.json << 'CONF'
      {
        "gateway": {
          "port": 18789,
          "bind": "lan",
          "mode": "local"
        }
      }
      CONF
      fi
      exec openclaw gateway --port 18789
    '';

    serviceConfig = {
      User = "openclaw";
      Group = "openclaw";
      WorkingDirectory = "/var/lib/openclaw";
      StateDirectory = "openclaw";
      Restart = "always";
      RestartSec = 5;
      # Hardening
      ProtectHome = "read-only";
      PrivateTmp = true;
      NoNewPrivileges = true;
    };
  };
}
