{ lib, ... }:
{
  services = {
    bird = {
      enable = lib.mkDefault true;
      checkConfig = false;
    };
  };
}
