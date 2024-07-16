{ lib, ... }:
{
  # This file was populated at runtime with the networking
  # details gathered from the active system.
  networking = {
    nameservers = [ "8.8.8.8" ];
    defaultGateway = "167.172.80.1";
    defaultGateway6 = {
      address = "";
      interface = "eth0";
    };
    dhcpcd.enable = false;
    usePredictableInterfaceNames = lib.mkForce false;
    interfaces = {
      eth0 = {
        ipv4.addresses = [
          {
            address = "167.172.91.53";
            prefixLength = 20;
          }
          {
            address = "10.15.0.6";
            prefixLength = 16;
          }
        ];
        ipv6.addresses = [
          {
            address = "fe80::2c49:57ff:fe35:c6e1";
            prefixLength = 64;
          }
        ];
        ipv4.routes = [
          {
            address = "167.172.80.1";
            prefixLength = 32;
          }
        ];
        ipv6.routes = [
          {
            address = "";
            prefixLength = 128;
          }
        ];
      };
      eth1 = {
        ipv4.addresses = [
          {
            address = "10.104.0.3";
            prefixLength = 20;
          }
        ];
        ipv6.addresses = [
          {
            address = "fe80::c0c5:3fff:fe8b:a705";
            prefixLength = 64;
          }
        ];
      };
    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="2e:49:57:35:c6:e1", NAME="eth0"
    ATTR{address}=="c2:c5:3f:8b:a7:05", NAME="eth1"
  '';
}
