{ config, pkgs, options, lib, ... }:

{
  services.restics = {
    server = {
      enable = true;
      port = 8000;
    };
  };
}
