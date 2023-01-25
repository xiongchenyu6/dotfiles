{ modulesPath, suites, profiles, ... }: {

  boot = {
    initrd = {
      kernelModules = [ "nvme" ];
      availableKernelModules = [ "ata_piix" "uhci_hcd" "xen_blkfront" ];
    };

    loader = {
      grub = {
        efiSupport = true;
        efiInstallAsRemovable = true;
        device = "nodev";
      };
    };

  };

  fileSystems = {
    "/boot" = {
      device = "/dev/disk/by-uuid/B331-7C58";
      fsType = "vfat";
    };
    "/" = {
      device = "/dev/mapper/ocivolume-root";
      fsType = "xfs";
    };
  };

  imports = suites.server-base ++ [
    (modulesPath + "/profiles/qemu-guest.nix")
    profiles.server-apps.webapps.gotify-server
    profiles.server-pkgs.nixos
    profiles.users.root.nixos
    # profiles.users."freeman.xiong"
  ];

  zramSwap.enable = true;

  boot = {
    cleanTmpDir = true;
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
