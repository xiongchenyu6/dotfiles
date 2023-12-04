{ lib, ... }: {
  services = {
    bird2 = {
      enable = lib.mkDefault true;
      checkConfig = false;
    };
  };
}
