{
  virtualisation = {
    # docker = { enable = true; };
    podman = {
      enable = true;
      dockerSocket.enable = true;
      defaultNetwork.settings.dns_enabled = true;
      dockerCompat = true;
      autoPrune = {
        enable = true;
        flags = ["--all" "--force"];
      };
    };
  };

  networking.firewall.checkReversePath = false;
}