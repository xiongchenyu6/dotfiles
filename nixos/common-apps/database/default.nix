{ config, pkgs, options, lib, ... }:

{
  services = {
    postgresql = {
      enable = false;
      authentication = ''
        local all all trust
      '';
    };
  };
}
