{ config, pkgs, options, lib, ... }:

{
  virtualisation = {
    docker = {
      enable = true;
      rootless = { enable = true; };
    };
    # virtualbox = { host = { enable = true; }; };
    # libvirtd.enable = true;
  };

  # users.users.freeman.extraGroups = [ "libvirtd" ];

  # networking.firewall.checkReversePath = false;

}
