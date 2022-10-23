{ config, pkgs, lib, ... }: {
  networking = { hostName = "mail"; };

  imports = [
    ./hardware-configuration.nix
    ../../nixos
    ../../nixos/server.nix
    ../../nixos/optional-apps/gitea
    ../../nixos/optional-apps/healthcheck.nix
    ../../nixos/optional-apps/calibre-web.nix
    ../../nixos/optional-apps/gotify-server.nix
  ];

  boot.cleanTmpDir = true;
  zramSwap.enable = true;

  boot = {
    #isContainer = true;
    kernel.sysctl = {
      "net.ipv4.ip_forward" = 1;
      "net.ipv6.conf.all.forwarding" = 1;
      "net.ipv6.conf.default.forwarding" = 1;

      "net.ipv4.conf.default.rp_filter" = 0;
      "net.ipv4.conf.all.rp_filter" = 0;
    };
  };

}
