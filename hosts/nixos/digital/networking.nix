{ lib, ... }:
{
  # This file was populated at runtime with the networking
  # details gathered from the active system.
  networking = {
    nameservers = [ "8.8.8.8" ];
    defaultGateway = "206.189.144.1";
    defaultGateway6 = {
      address = "2400:6180:0:d0::1";
      interface = "eth0";
    };
    dhcpcd.enable = false;
    usePredictableInterfaceNames = lib.mkForce false;
    interfaces = {
      eth0 = {
        ipv4.addresses = [
          {
            address = "206.189.156.130";
            prefixLength = 20;
          }
          {
            address = "10.15.0.5";
            prefixLength = 16;
          }
        ];
        ipv6.addresses = [
          {
            address = "2400:6180:0:d0::1028:a001";
            prefixLength = 64;
          }
          {
            address = "fe80::b6:2dff:fed3:ceca";
            prefixLength = 64;
          }
        ];
        ipv4.routes = [
          {
            address = "206.189.144.1";
            prefixLength = 32;
          }
        ];
        ipv6.routes = [
          {
            address = "2400:6180:0:d0::1";
            prefixLength = 128;
          }
        ];
      };
      eth1 = {
        ipv4.addresses = [
          {
            address = "10.104.0.2";
            prefixLength = 20;
          }
        ];
        ipv6.addresses = [
          {
            address = "fe80::b8da:96ff:fefa:ea8c";
            prefixLength = 64;
          }
        ];
      };
    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="02:b6:2d:d3:ce:ca", NAME="eth0"
    ATTR{address}=="ba:da:96:fa:ea:8c", NAME="eth1"
  '';
}
