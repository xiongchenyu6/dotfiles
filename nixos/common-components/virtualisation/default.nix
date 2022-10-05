{ config, pkgs, options, lib, ... }:

{
  virtualisation = {
    docker = {
      enable = true;
      rootless = {
        enable = true;
      };
    };
    virtualbox = {
      host = {
        enable = true;
      };
    };
  };

}
