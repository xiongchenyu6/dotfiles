{ config, pkgs, lib, ... }:

{
  # Hashtopolis server configuration for oracle-arm-002
  services.hashtopolis-server = {
    enable = true;

    # Network configuration
    listenAddress = "0.0.0.0";  # Listen on all interfaces
    port = 8080;  # Internal port for PHP server

    # Data storage
    dataDir = "/var/lib/hashtopolis";

    # Admin credentials (should be changed after first login)
    # Consider using age-encrypted secrets for production
    adminUser = "admin";
    adminPassword = "InitialPassword123!";  # CHANGE THIS IMMEDIATELY AFTER DEPLOYMENT

    # Database configuration
    database = {
      createLocally = true;  # Use local MariaDB
      host = "localhost";
      port = 3306;
      name = "hashtopolis";
      user = "hashtopolis";
      password = "SecureDbPassword456!";  # Consider using age-encrypted secrets
    };

    # Additional environment variables if needed
    extraEnvVars = ''
      # Add any extra environment variables here
      # MAX_UPLOAD_SIZE=100M
      # SESSION_TIMEOUT=3600
    '';

    # Nginx reverse proxy configuration
    nginx = {
      enable = true;
      virtualHost = "hashtopolis.xiongchenyu.dpdns.org";  # Cloudflare domain
    };
  };

  # Firewall configuration
  networking.firewall = {
    allowedTCPPorts = [
      80    # HTTP (nginx)
      443   # HTTPS (nginx with ACME/Let's Encrypt)
      # 8080 is not exposed externally when nginx is enabled
    ];
  };

  # Configure ACME/Let's Encrypt for SSL
  security.acme = {
    acceptTerms = true;
    defaults.email = "xiongchenyu6@gmail.com";  # Update with your email
  };

  # Configure nginx SSL
  services.nginx.virtualHosts."hashtopolis.xiongchenyu.dpdns.org" = {
    forceSSL = true;
    enableACME = true;
  };

  # System packages for debugging/management
  environment.systemPackages = with pkgs; [
    mariadb  # For database management
    htop     # System monitoring
    ncdu     # Disk usage analysis
  ];
}