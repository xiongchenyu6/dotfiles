{ config, pkgs, lib, ... }: {
  networking = let
    file-path = builtins.split "/" (toString ./.);
    hostName = lib.last file-path;
  in { inherit hostName; };

  imports = [
    ./hardware-configuration.nix
    ../../nixos
    ../../nixos/server.nix
    ../../nixos/optional-apps/gitea
    ../../nixos/optional-apps/healthcheck.nix
    ../../nixos/optional-apps/calibre-web.nix
    ../../nixos/optional-apps/gotify-server.nix
    # ../../nixos/optional-apps/hercules-ci-agent.nix
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
