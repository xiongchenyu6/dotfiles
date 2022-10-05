{ config, pkgs, options, lib, ... }:

{
  services.restic = {
    server = {
      enable = false;
      listenAddress = "localhost:8001";
    };
  };
}
