{ config, pkgs, options, lib, ... }:

{
  services = {
    postgresql = {
      enable = true;
      authentication = ''
        local all all trust
      '';
    };
  };
}
