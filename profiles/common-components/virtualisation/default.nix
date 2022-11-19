{
  virtualisation = {
    docker = {
      enable = true;
      rootless = {enable = true;};
    };
    # virtualbox = { host = { enable = true; }; };
    libvirtd.enable = true;
  };

  networking.firewall.checkReversePath = false;
}
