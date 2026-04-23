{
  config,
  pkgs,
  lib,
  ...
}:
{
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

  # Hashtopolis agent configuration
  services.hashtopolis-agent = {
    enable = true;

    # Server configuration
    serverUrl = "https://hashtopolis.panda.qzz.io/api/server.php";

    # Use SOPS secret for voucher - module handles UUID preservation natively
    voucherFile = config.sops.secrets."hashtopolis/agent/voucher".path;

    # Don't use native hashcat - let the server manage binaries
    # The module now includes nix-ld support for downloaded binaries
    useNativeHashcat = true;

    # Enable GPU support (Legion laptop has NVIDIA GPU)
    deviceTypes = [
      "cpu"
      "gpu"
    ];

    # Auto-start the agent
    autoStart = true;
    restartOnFailure = true;

    # Resource limits
    memoryLimit = "8G"; # Limit memory usage to 8GB
    cpuQuota = 100; # Use up to 80% CPU

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
}
