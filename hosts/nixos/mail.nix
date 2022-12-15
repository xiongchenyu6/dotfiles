{ modulesPath, suites, profiles, config, ... }: {
  sops.secrets."oci-arm-host-capacity" = { };

  services = {
    oci-arm-host-capacity = {
      enable = true;
      envPath = config.sops.secrets."oci-arm-host-capacity".path;
    };
  };

  boot.loader.grub.device = "/dev/vda";
  boot.initrd.availableKernelModules = [ "ata_piix" "uhci_hcd" "xen_blkfront" ];

  boot.initrd.kernelModules = [ "nvme" ];
  fileSystems."/" = {
    device = "/dev/vda2";
    fsType = "ext4";
  };

  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
    profiles.optional-apps.gitea
    profiles.optional-apps.healthcheck
    profiles.optional-apps.calibre-web
    profiles.optional-apps.gotify-server
    profiles.optional-apps.zammad
    profiles.server-pkgs.nixos
    profiles.users.root.nixos
    profiles.users."freeman.xiong"
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
