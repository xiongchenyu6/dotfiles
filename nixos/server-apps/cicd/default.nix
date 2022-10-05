{ config, pkgs, options, lib, ... }:

{
  services = {
    hydra = {
      enable = false;
      hydraURL = "http://localhost:3000"; # externally visible URL
      notificationSender = "hydra@localhost"; # e-mail of hydra service
      # a standalone hydra will require you to unset the buildMachinesFiles list to avoid using a nonexistant /etc/nix/machines
      buildMachinesFiles = [ ];
      # you will probably also want, otherwise *everything* will be built from scratch
      useSubstitutes = true;
      extraConfig = ''
        <dynamicruncommand>
          enable = 1
        </dynamicruncommand>
      '';
    };
  };
}
