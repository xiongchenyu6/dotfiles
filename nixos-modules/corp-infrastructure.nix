{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:
{
  imports = [
    ./samba.nix
    ./fleet.nix
  ];

  # Basic system configuration for corp.autolife.ai domain
  networking = {
    domain = "corp.autolife.ai";
    firewall.enable = true;
  };

  # Samba configuration for Windows device management
  services.samba.settings = {
    global = {
      workgroup = lib.mkDefault "AUTOLIFE";
      "server string" = lib.mkDefault "AutoLife Corp Server";
      # Enable Active Directory support
      realm = "CORP.AUTOLIFE.AI";
      # Domain Master Browser settings for corporate network
      "domain master" = lib.mkDefault "yes";
      "local master" = lib.mkDefault "yes";
      "preferred master" = lib.mkDefault "yes";
      "os level" = lib.mkDefault "65";
      # Enable browsing for Windows network discovery
      "browse list" = lib.mkDefault "yes";
      # Disable printing
      "disable spoolss" = "yes";
      "load printers" = "no";
      "printcap name" = "/dev/null";
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
    software = {
      path = "/srv/samba/software";
      browseable = "yes";
      "read only" = "yes";
      "guest ok" = "yes";
      comment = "Corporate Software Repository";
    };
  };

  # Fleet configuration for device management
  services.fleet = {
    enable = true;
    host = "0.0.0.0";
    port = 8080;
    mysqlDatabase = "fleet_corp";
    mysqlUser = "fleet_corp";
    logLevel = "info";
    extraConfig = {
      # Fleet-specific configuration for corp environment
      osquery = {
        node_key_size = 24;
        label_update_interval = "30m";
        detail_update_interval = "30m";
      };
      server = {
        # Custom branding for AutoLife Corp
        server_url = "https://fleet.corp.autolife.ai";
      };
      app = {
        token_key_size = 24;
        invite_token_validity_period = "5d";
      };
    };
  };

  # Additional directories for corp infrastructure
  systemd.tmpfiles.rules = [
    "d /srv/samba/corp-shared 0775 samba samba"
    "d /srv/samba/software 0755 samba samba"
  ];

  # DNS configuration for corp domain
  services.bind = lib.mkIf config.services.bind.enable {
    zones."corp.autolife.ai" = {
      master = true;
      file = pkgs.writeText "corp.autolife.ai.zone" ''
        $TTL 86400
        @       IN      SOA     ns1.corp.autolife.ai. admin.corp.autolife.ai. (
                        $(date +%Y%m%d%H)      ; Serial
                        3600            ; Refresh
                        1800            ; Retry
                        604800          ; Expire
                        86400 )         ; Minimum TTL

        ; Name servers
        @       IN      NS      ns1.corp.autolife.ai.

        ; A records
        ns1     IN      A       ${config.networking.interfaces.eth0.ipv4.addresses.address or "127.0.0.1"}
        fleet   IN      A       ${config.networking.interfaces.eth0.ipv4.addresses.address or "127.0.0.1"}
        samba   IN      A       ${config.networking.interfaces.eth0.ipv4.addresses.address or "127.0.0.1"}
      '';
    };
  };

  # Security configurations for corp environment
  security = {
    # Enable sudo for fleet management
    sudo.extraRules = [
      {
        users = [ "fleet" ];
        commands = [
          {
            command = "${pkgs.systemd}/bin/systemctl restart fleet";
            options = [ "NOPASSWD" ];
          }
        ];
      }
    ];
  };

  # Environment packages for corp management
  environment.systemPackages = with pkgs; [
    samba
    # Add osquery if available from xiongchenyu6 packages
  ] ++ lib.optionals (inputs ? xiongchenyu6) (
    with inputs.xiongchenyu6.packages.${pkgs.system}; [
      # Add any fleet-related packages from xiongchenyu6 if available
    ]
  );
}