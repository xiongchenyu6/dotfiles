{
  config,
  lib,
  pkgs,
  inputs,
  shares,
  mylib,
  ...
}:
{
  imports = with inputs; [
    srvos.nixosModules.server
    srvos.nixosModules.mixins-nginx
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
    ../../nixos-modules/corp-infrastructure.nix
    ../../nixos-modules/acme
  ];

  # Hostname and domain configuration
  networking = {
    hostName = "corp-server";
    domain = "autolife.ai";
  };

  # ACME configuration for autolife.ai
  mylib.acme = {
    certs = {
      "autolife.ai" = {
        domain = "autolife.ai";
        extraDomainNames = [ "*.autolife.ai" ];
        email = "xiongchenyu6@gmail.com";
        dnsProvider = "cloudflare"; # Adjust based on your DNS provider
        # credentialsFile should be configured in secrets
      };
    };
  };

  # Nginx configuration for corp services
  services.nginx = {
    enable = true;

    virtualHosts = {
      # Fleet management interface
      "fleet.autolife.ai" = {
        useACMEHost = "autolife.ai";
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://127.0.0.1:8080";
          proxyWebsockets = true;
          extraConfig = ''
            proxy_set_header Host $host;
            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
            proxy_set_header X-Forwarded-Proto $scheme;
          '';
        };
      };

      # Samba web management (if needed)
      "samba.autolife.ai" = {
        useACMEHost = "autolife.ai";
        forceSSL = true;
        root = "/var/www/samba";
        locations."/" = {
          index = "index.html";
        };
      };

    };
  };

  # Fleet service configuration
  services.fleet = {
    enable = true;
    host = "127.0.0.1";
    port = 8080;
    mysqlDatabase = "fleet_corp";
    mysqlUser = "fleet_corp";
    # MySQL password should be configured via SOPS
    # mysqlPasswordFile = config.sops.secrets."fleet/mysql-password".path;
    logLevel = "info";
  };

  # Samba configuration for Windows devices
  services.samba = {
    enable = true;
    shares = {
      corp-public = {
        path = "/srv/samba/corp-public";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "yes";
        comment = "AutoLife Corp Public Share";
      };
      corp-software = {
        path = "/srv/samba/corp-software";
        browseable = "yes";
        "read only" = "yes";
        "guest ok" = "yes";
        comment = "Corporate Software Repository";
      };
    };
  };

  # Firewall configuration
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [
      80 # HTTP
      443 # HTTPS
      139 # NetBIOS Session Service
      445 # SMB
      8080 # Fleet (internal)
    ];
    allowedUDPPorts = [
      137 # NetBIOS Name Service
      138 # NetBIOS Datagram Service
    ];
  };

  # System packages
  environment.systemPackages =
    with pkgs;
    [
      samba
      mysql
      redis
      htop
      curl
      wget
      git
    ]
    ++ lib.optionals (inputs ? xiongchenyu6) (
      with inputs.xiongchenyu6.packages.${pkgs.system};
      [
        # Add fleet or osquery packages if available
      ]
    );

  # Enable required services
  services.mysql = {
    enable = true;
    package = pkgs.mariadb;
    initialDatabases = [
      { name = "fleet_corp"; }
    ];
  };

  services.redis.servers."" = {
    enable = true;
    port = 6379;
  };

  # Create web directories
  systemd.tmpfiles.rules = [
    "d /var/www 0755 nginx nginx"
    "d /var/www/corp 0755 nginx nginx"
    "d /var/www/samba 0755 nginx nginx"
    "d /srv/samba/corp-public 0775 samba samba"
    "d /srv/samba/corp-software 0755 samba samba"
  ];

  # System configuration
  system.stateVersion = "24.11";

  # Enable systemd-resolved for proper DNS
  services.resolved = {
    enable = true;
    domains = [ "autolife.ai" ];
  };
}
