{
  virtualisation = {
    podman = {
      enable = true;
      dockerSocket.enable = true;
      defaultNetwork.dnsname.enable = true;
      dockerCompat = true;
    };
  };

  networking.firewall.checkReversePath = false;
}
