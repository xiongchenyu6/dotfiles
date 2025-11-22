{ config, pkgs, lib, ... }:
let
  secrets = config.sops.secrets;
in

{
  # Import the Hashtopolis agent module from NUR packages
  imports = [
    # The module is imported via xiongchenyu6 flake input
  ];

  # SOPS secrets configuration
  sops.secrets = {
    "hashtopolis/agent/voucher" = {
      sopsFile = ../../secrets/game-hashtopolis.yaml;
      owner = "hashtopolis-agent";
      group = "hashtopolis-agent";
      mode = "0400";
      restartUnits = [ "hashtopolis-agent.service" ];
    };
  };

  # Create custom config file with voucher from SOPS
  sops.templates."hashtopolis-config.json" = {
    content = ''
      {
        "url": "https://hashtopolis.xiongchenyu.dpdns.org/api/server.php",
        "voucher": "${config.sops.placeholder."hashtopolis/agent/voucher"}",
        "uuid": "",
        "files-path": "/var/lib/hashtopolis-agent/files",
        "hashlist-path": "/var/lib/hashtopolis-agent/hashlists",
        "zaps-path": "/var/lib/hashtopolis-agent/zaps",
        "crackers-path": "/var/lib/hashtopolis-agent/crackers",
        "prince-path": "/var/lib/hashtopolis-agent/prince",
        "preprocessors-path": "/var/lib/hashtopolis-agent/preprocessors",
        "use-native-hashcat": true,
        "allow-piping": false,
        "disable-update": true
      }
    '';
    owner = "hashtopolis-agent";
    group = "hashtopolis-agent";
    mode = "0400";
  };


  # Hashtopolis agent configuration
  services.hashtopolis-agent = {
    enable = true;

    # Server configuration
    serverUrl = "https://hashtopolis.xiongchenyu.dpdns.org/api/server.php";

    # Voucher will be loaded from SOPS secret
    # Using a placeholder that will be overridden at runtime
    voucher = "PLACEHOLDER_VOUCHER";

    # UUID will be automatically generated after registration
    uuid = "";

    # Use system hashcat instead of downloading
    useNativeHashcat = true;
    hashcatPackage = pkgs.hashcat;

    # Enable GPU support (Legion laptop has NVIDIA GPU)
    deviceTypes = [ "cpu" "gpu" ];

    # Auto-start the agent
    autoStart = true;
    restartOnFailure = true;

    # Resource limits
    memoryLimit = "8G";  # Limit memory usage to 8GB
    cpuQuota = 80;  # Use up to 80% CPU

    # Data directory
    dataDir = "/var/lib/hashtopolis-agent";
  };

  # Ensure NVIDIA drivers are available for GPU cracking
  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      nvidia-vaapi-driver
      libva-vdpau-driver
      libvdpau-va-gl
    ];
  };

  # Open firewall for outgoing connections (not needed for agent, but good to document)
  # The agent only makes outbound connections to the server
  # networking.firewall.allowedTCPPorts = [ ];

  # Override systemd service to use SOPS template config
  systemd.services.hashtopolis-agent = {
    environment = {
      HASHTOPOLIS_CONFIG = config.sops.templates."hashtopolis-config.json".path;
    };

    # Override the prestart script to use our config
    serviceConfig.ExecStartPre = lib.mkForce (
      let
        preStartScript = pkgs.writeShellScript "hashtopolis-agent-prestart" ''
          # Copy SOPS config file
          cp ${config.sops.templates."hashtopolis-config.json".path} /var/lib/hashtopolis-agent/config.json
          chown hashtopolis-agent:hashtopolis-agent /var/lib/hashtopolis-agent/config.json
          chmod 640 /var/lib/hashtopolis-agent/config.json

          # Link hashcat
          ln -sf ${pkgs.hashcat}/bin/hashcat /var/lib/hashtopolis-agent/crackers/hashcat
          chown -h hashtopolis-agent:hashtopolis-agent /var/lib/hashtopolis-agent/crackers/hashcat
        '';
      in "+${preStartScript}"
    );
  };
}