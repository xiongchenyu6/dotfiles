{ config, pkgs, lib, modulesPath, suites, profiles, ... }: {

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
    profiles.optional-apps.gitea
    profiles.optional-apps.healthcheck
    profiles.optional-apps.calibre-web
    profiles.optional-apps.gotify-server
    profiles.users.root
    profiles.users.freeman
  ] ++ suites.server-base;

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
