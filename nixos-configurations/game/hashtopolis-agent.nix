{ config, pkgs, lib, ... }:

{
  # Import the Hashtopolis agent module from NUR packages
  imports = [
    # The module is imported via xiongchenyu6 flake input
  ];

  # Hashtopolis agent configuration
  services.hashtopolis-agent = {
    enable = true;

    # Server configuration
    serverUrl = "https://hashtopolis.xiongchenyu.dpdns.org/api/server.php";

    # Voucher for initial registration
    # This will be cleared after first successful registration
    voucher = "X18Tg93M";

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
}