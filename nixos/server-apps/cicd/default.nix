{ config, pkgs, options, lib, domain, ... }:

{
  services = {
    hydra = {
      enable = false;
      hydraURL = "https://hydra.inner.${domain}"; # externally visible URL
      notificationSender = "hydra@mail.freeman.engineer"; # e-mail of hydra service
      # a standalone hydra will require you to unset the buildMachinesFiles list to avoid using a nonexistant /etc/nix/machines
      buildMachinesFiles = [ ];
      # you will probably also want, otherwise *everything* will be built from scratch
      useSubstitutes = true;
      # listenHost = "hydra.inner.${domain}";
      extraConfig = ''
        <dynamicruncommand>
          enable = 1
        </dynamicruncommand>
      '';
    };
  };
}
