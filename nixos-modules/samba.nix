{
  config,
  lib,
  pkgs,
  ...
}:
{
  services.samba-wsdd.enable = true; # WS-Discovery daemon
  services.samba = {
    enable = true;
    openFirewall = true;
    settings = {
      global = {
        workgroup = lib.mkDefault "CORP";
        "server string" = lib.mkDefault "Samba Server %v";
        "netbios name" = config.networking.hostName;
        security = "user";
        # Domain Master Browser settings
        "domain master" = "yes";
        "local master" = "yes";
        "preferred master" = "yes";
        "os level" = "65";
        # Enable browsing
        "browse list" = "yes";
        # Disable printer services
        "disable spoolss" = "yes";
        "load printers" = "no";
        "printcap name" = "/dev/null";
        # Enable wins support
        "wins support" = "yes";
        # DNS proxy
        "dns proxy" = "no";
        # Log configuration
        "log file" = "/var/log/samba/log.%m";
        "max log size" = "1000";
        "log level" = "0";
      };
      public = {
        path = "/srv/samba/public";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "yes";
        "create mask" = "0664";
        "directory mask" = "0775";
        "force user" = "samba";
        "force group" = "samba";
      };
      corp = {
        path = "/srv/samba/corp";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "no";
        "create mask" = "0664";
        "directory mask" = "0775";
        "force user" = "samba";
        "force group" = "samba";
      };
    };
  };

  # Create samba user and group
  users.users.samba = {
    isSystemUser = true;
    group = "samba";
    createHome = false;
    description = "Samba service user";
  };
  users.groups.samba = {};

  # Create directories
  systemd.tmpfiles.rules = [
    "d /srv/samba 0755 samba samba"
    "d /srv/samba/public 0775 samba samba"
    "d /srv/samba/corp 0775 samba samba"
  ];

  # Firewall configuration
  networking.firewall = {
    allowedTCPPorts = [ 139 445 ];
    allowedUDPPorts = [ 
      137   # NetBIOS Name Service
      138   # NetBIOS Datagram Service
    ];
  };

  # Enable NetBIOS name resolution
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    publish = {
      enable = true;
      addresses = true;
      domain = true;
      hinfo = true;
      userServices = true;
      workstation = true;
    };
    extraServiceFiles = {
      smb = ''
        <?xml version="1.0" standalone='no'?>
        <!DOCTYPE service-group SYSTEM "avahi-service.dtd">
        <service-group>
          <name replace-wildcards="yes">%h</name>
          <service>
            <type>_smb._tcp</type>
            <port>445</port>
          </service>
        </service-group>
      '';
    };
  };

  # Enable mDNS
  services.resolved.enable = true;
}