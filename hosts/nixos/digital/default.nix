{ modulesPath, suites, profiles, lib, ... }: {

  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
    profiles.server-pkgs.nixos
    profiles.users.root.nixos
    profiles.users."freeman.xiong"
  ] ++ suites.server-base;

  boot = {
    loader.grub.device = "/dev/vda";
    initrd.availableKernelModules =
      [ "ata_piix" "uhci_hcd" "xen_blkfront" "vmw_pvscsi" ];
    initrd.kernelModules = [ "nvme" ];
  };
  fileSystems."/" = {
    device = "/dev/vda1";
    fsType = "ext4";
  };

  boot.cleanTmpDir = true;
  zramSwap.enable = true;
  services.openssh.enable = true;

  networking = {
    nameservers = [ "8.8.8.8" ];
    defaultGateway = "128.199.192.1";
    defaultGateway6 = "2400:6180:0:d0::1";
    dhcpcd.enable = false;
    usePredictableInterfaceNames = lib.mkForce false;
    interfaces = {
      eth0 = {
        ipv4.addresses = [
          {
            address = "128.199.202.171";
            prefixLength = 18;
          }
          {
            address = "10.15.0.5";
            prefixLength = 16;
          }
        ];
        ipv6.addresses = [
          {
            address = "2400:6180:0:d0::13de:f001";
            prefixLength = 64;
          }
          {
            address = "fe80::546b:b4ff:fe1d:eec1";
            prefixLength = 64;
          }
        ];
        ipv4.routes = [{
          address = "128.199.192.1";
          prefixLength = 32;
        }];
        ipv6.routes = [{
          address = "2400:6180:0:d0::1";
          prefixLength = 128;
        }];
      };

    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="56:6b:b4:1d:ee:c1", NAME="eth0"
    ATTR{address}=="1e:f8:ca:93:78:fa", NAME="eth1"
  '';
}
