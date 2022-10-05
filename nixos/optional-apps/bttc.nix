{ config, pkgs, options, lib, ... }:

{
  services = {
    bttc = {
      enable = false;
      prometheus = true;
    };
  };
}
