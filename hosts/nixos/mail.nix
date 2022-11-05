{ config, pkgs, lib, modulesPath, ... }: {

  boot.loader.grub.device = "/dev/vda";
  boot.initrd.availableKernelModules = [ "ata_piix" "uhci_hcd" "xen_blkfront" ];
  boot.initrd.kernelModules = [ "nvme" ];
  fileSystems."/" = {
    device = "/dev/vda2";
    fsType = "ext4";
  };

  # networking = let
  #   file-path = builtins.split "/" (toString ./.);
  #   hostName = lib.last file-path;
  # in { hostName = "mail"; };

  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
    ../../profiles
    ../../profiles/server.nix
    ../../profiles/optional-apps/gitea
    ../../profiles/optional-apps/healthcheck.nix
    ../../profiles/optional-apps/calibre-web.nix
    ../../profiles/optional-apps/gotify-server.nix
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
