{ config, pkgs, options, lib, ... }:
let
  share = import ../../common/share.nix;
in

{
  age.secrets.office_wg_pk.file = ../../common/secrets/office_wg_pk.age;

  networking = {
    firewall = {
      allowedTCPPorts = [ 179 ];
      allowedUDPPorts = [ 179 33434 ];
      enable = true;
    };

    networkmanager = { enable = true; };
    enableIPv6 = true;
    #hostName = "office"; # Define your hostname.
    # Enable networking
    #networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.
    wg-quick = {
      interfaces = {
        wg_office = {
          privateKeyFile = config.age.secrets.office_wg_pk.path;
          address = [ "172.22.240.98/27" "fe80::101/64" "fd48:4b4:f3::2/48" ];
          dns = [ "fe80::100%wg_office" "172.22.240.97" "1.1.1.1" ];
          peers = [{
            endpoint = "freeman.engineer:22616";
            publicKey = share.tc.wg.public-key;
            persistentKeepalive = 30;
            allowedIPs = [
              "10.0.0.0/8"
              "172.20.0.0/14"
              "172.31.0.0/16"
              "fd00::/8"
              "fe80::/10"
              "fd48:4b4:f3::/48"
            ];
          }];
        };
      };
    };
    extraHosts = ''
      #  127.0.0.1 freeman.engineer
    '';
  };

}
