{
  virtualisation = {
    # docker = { enable = true; };
    podman = {
      enable = true;
      dockerSocket.enable = true;
      defaultNetwork.settings.dns_enabled = true;
      dockerCompat = true;
    };
  };

  networking.firewall.checkReversePath = false;
}
