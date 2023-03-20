{ suites, profiles, config, modulesPath, lib, pkgs, ... }: {

  imports = [
    profiles.core.nixos
    profiles.server-pkgs.nixos
    profiles.users.root.nixos
    (modulesPath + "/virtualisation/digital-ocean-config.nix")
    profiles.users."freeman.xiong"
  ] ++ suites.server-base ++ suites.client-network;

  sops.secrets."wireguard/digital" = { };
  boot = {
    kernel = {
      sysctl = {
        "net.ipv4.ip_forward" = 1;
        # "net.ipv4.conf.default.rp_filter" = 0;
        # "net.ipv4.conf.all.rp_filter" = 0;
        # "net.ipv4.conf.default.forwarding" = 1;
        # "net.ipv4.conf.all.forwarding" = 1;

        # "net.ipv6.conf.all.accept_redirects" = 0;
        # "net.ipv6.conf.default.forwarding" = 1;
        # "net.ipv6.conf.all.forwarding" = 1;
      };
    };
  };

  networking = {
    # interfaces = {
    #   lo = {
    #     ipv4 = {
    #       addresses = [{
    #         address = "172.22.240.99";
    #         prefixLength = 32;
    #       }];
    #     };
    #     ipv6 = {
    #       addresses = [{
    #         address = "fd48:4b4:f3::3";
    #         prefixLength = 128;
    #       }];
    #     };
    #   };
    # };

    firewall = { enable = false; };
    nameservers = [ "8.8.8.8" ];
    wg-quick = {
      interfaces = {
        wg_mail = {
          privateKeyFile = config.sops.secrets."wireguard/digital".path;
          listenPort = 22616;
          table = "off";
          address = [ "fe80::102" ];
          postUp = ''
            ${pkgs.iproute2}/bin/ip addr add dev wg_mail 172.22.240.99 peer 172.22.240.97
            # ${pkgs.iproute2}/bin/ip addr add dev wg_mail fe80::102 peer fd48:4b4:f3::1
          '';

          peers = [{
            endpoint = "mail.freeman.engineer:22617";
            publicKey = profiles.share.hosts-dict.mail.wg.public-key;
            allowedIPs = [
              "10.0.0.0/8"
              "172.20.0.0/14"
              "172.31.0.0/16"
              "fd00::/8"
              "fe80::/10"
              "fd48:4b4:f3::/48"
              "ff02::1:6/128"
            ];
          }];
        };
      };
    };
  };
  services = {
    babeld.enable = false;
    babeld.interfaces = { wg_mail = { split-horizon = "auto"; }; };
    babeld.extraConfig = ''
      redistribute if ens3 deny
      redistribute if ens4 deny
    '';

    bird2 = {
      # enable = false;
      config = lib.mine.bird2-inner-config "172.22.240.99" "fd48:4b4:f3::3";
    };
  };
}
