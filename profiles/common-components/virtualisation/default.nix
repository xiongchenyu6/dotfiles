{
  virtualisation = {
    podman = {
      enable = true;
    };
  };

  networking.firewall.checkReversePath = false;
}
