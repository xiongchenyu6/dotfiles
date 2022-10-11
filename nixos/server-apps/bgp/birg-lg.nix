{ config, pkgs, lib, ... }:
let
  common-files-path = ../../common;
  secret-files-paht = common-files-path + "/secrets";
  script = import ../../dn42/update-roa.nix { inherit pkgs; };
  share = import (common-files-path + /share.nix);
in

{
  services = {
    bird-lg = {
      package = pkgs.symlinkJoin {
        name = "bird-lg";
        paths = with pkgs; [ bird-lg-go bird-lgproxy-go ];
      };
      proxy = {
        enable = true;
        birdSocket = "/var/run/bird/bird.ctl";
        listenAddress = "0.0.0.0:8000";
        allowedIPs = [ "127.0.0.1" "43.156.66.157" "14.100.28.225" ];
      };
      frontend = {
        domain = "inner." + config.networking.domain;
        enable = true;
        netSpecificMode = "dn42";
        servers = [ "sg1" ];
        nameFilter = "^ospf";
        protocolFilter = [ "bgp" "ospf" "static" ];
        whois = "whois.burble.dn42";
        # titleBrand = "Freeman dn42 bird-lg";
        dnsInterface = "asn.lantian.dn42";
        navbar = {
          # brand = "Freeman dn42 bird-lg";
        };
      };
    };
  };
}
