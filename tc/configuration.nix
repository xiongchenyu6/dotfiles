{ pkgs, lib, secret, symlinkJoin, xddxdd, ... }:
let
  script = pkgs.writeShellScriptBin "update-roa" ''
    mkdir -p /etc/bird/
    ${pkgs.curl}/bin/curl -sfSLR {-o,-z}/etc/bird/roa_dn42_v6.conf https://dn42.burble.com/roa/dn42_roa_bird2_6.conf
    ${pkgs.curl}/bin/curl -sfSLR {-o,-z}/etc/bird/roa_dn42.conf https://dn42.burble.com/roa/dn42_roa_bird2_4.conf
    ${pkgs.bird2}/bin/birdc c 
    ${pkgs.bird2}/bin/birdc reload in all
  '';
  domain = "freeman.engineer";
in {
  imports = [ ./hardware-configuration.nix ];

  boot.cleanTmpDir = true;
  boot.kernel.sysctl = {
    "net.ipv4.ip_forward" = 1;
    "net.ipv6.conf.all.forwarding" = 1;
    "net.ipv6.conf.default.forwarding" = 1;

    "net.ipv4.conf.default.rp_filter" = 0;
    "net.ipv4.conf.all.rp_filter" = 0;
  };
  zramSwap.enable = true;
  networking = {
    hostName = "mail";
    firewall = {
      enable = true;
      allowedTCPPorts = [
        80 # ui
        443
        8000
      ];
      allowedUDPPorts = [ 22616 23396 21816 ];
      extraCommands = ''
        iptables -A FORWARD -i wg_freeman -j ACCEPT
        iptables -t nat -A POSTROUTING -o ens5 -j MASQUERADE 
      '';
    };
  };

  users.users.root.openssh.authorizedKeys.keys = [
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCzzKV5IF7ekz9oJQ37nbaUNhXKkQ4KzJiDOYVRVroFq+LEJZHqNxe/Lt1Z1cKvFjRruu6f3clzqRargKlmqbO1d8mJZy0R9TbKQxleEZZq2cZJemX99xrkiu9keBF2qhohwn28v0JUuUyjNo188/YyS1tocoWFNZtp7qPiK8HRF7LQQ99nOa3zGmZJQL5Rvs2RFTFMGhiehsq8aXFuTZNejjivl5BFJjzxoVqZSbB8//lwsGZWpU5Ue54KV51UTv+9wDh2myuyenP/ZbdK9UZo9abCIeI52F9QbGJtjz6cOKG6oz67x06EYxvD/HKJ/uPuisy/cu+rPInmaF5AZTnd skey-p1r300u3"
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC3LHhrdC+Mor6lM9U0fxyJ2WCn4CzNUZPyOP8ACQpAl5bADYY8ici2SbRD6y0dZnwNvSUJKw090HXiPOgKCYrfPQPX4IOgiPLqPBJq0JCI7w7/pewRmg1bd/5k5BC8C5x0P2H63DovDXEnyIJxqZnVWZDjfhysGEVGueoYBxeDAHHBZLwGGxW36oX8OmfiTGDmMrHWqQxKpluR6KIbe4aFML+ZIol0Vy6+244gREZZXn6xTAoCxRGghaEnOf5X3SivKOJHLTDpAXI7JYesepHHyCPd+OXH2VzSVj0qqOtzb5t6mNHkM4wC9HhTqPT/KWuxjv9HcpXjag9ZGuby/LxOo+6knb5a7VtRm0GxvbBptNS5Frlrl9HNwARiqSmiaSvOWydrZYKV0/ClIYdA7f4DMUc46KIP+wHqLXO2oBe5I4sK4TesmOxCYezi2ti/T4sC/e4Hlvgc/luvS6p0GdTtZ0wQLMmqz2u79LVRpjQMFygLa0IQXFo7c+0FqB7Et8M= skey-21jfw3n7"
  ];

  systemd = {
    network = {
      enable = true;
      netdevs = {
        "freeman" = {
          netdevConfig = {
            Kind = "wireguard";
            Name = "wg_freeman";
          };
          wireguardConfig = {
            PrivateKeyFile = pkgs.writeText "wg0-priv" secret.my.wg.private-key;
            ListenPort = 22616;
          };
          wireguardPeers = [{
            wireguardPeerConfig = {
              PublicKey = secret.freeman.wg.public-key;
              AllowedIPs = [ "172.22.240.98/32" ];
            };
          }];
        };
        "theresa" = {
          netdevConfig = {
            Kind = "wireguard";
            Name = "wg_theresa";
          };
          wireguardConfig = {
            PrivateKeyFile = pkgs.writeText "wg0-priv" secret.my.wg.private-key;
            ListenPort = 23396;
          };
          wireguardPeers = [{
            wireguardPeerConfig = {
              Endpoint = "cn2.dn42.theresa.cafe:22616";
              PublicKey = "MqKkzCwYfOg8Fc/pRRctLW3jS72ACBDQr8ZF10sZ614=";
              AllowedIPs = [
                "10.0.0.0/8"
                "172.20.0.0/14"
                "172.31.0.0/16"
                "fd00::/8"
                "fe80::/64"
              ];
            };
          }];
        };
        "potat0" = {
          netdevConfig = {
            Kind = "wireguard";
            Name = "wg_potat0";
          };
          wireguardConfig = {
            PrivateKeyFile = pkgs.writeText "wg0-priv" secret.my.wg.private-key;
            ListenPort = 21816;
          };
          wireguardPeers = [{
            wireguardPeerConfig = {
              Endpoint = "us1.dn42.potat0.cc:22616";
              PublicKey = "LUwqKS6QrCPv510Pwt1eAIiHACYDsbMjrkrbGTJfviU=";
              AllowedIPs = [
                "10.0.0.0/8"
                "172.20.0.0/14"
                "172.31.0.0/16"
                "fd00::/8"
                "fe80::/64"
              ];
            };
          }];
        };
      };
      networks = {
        "theresa" = {
          matchConfig = { Name = "wg_theresa"; };
          networkConfig = {
            DHCP = "no";
            IPv6AcceptRA = false;
            IPForward = "yes";
            KeepConfiguration = "yes";
          };
          addresses = [
            {
              addressConfig = {
                Address = "172.22.240.97/32";
                Peer = "172.22.162.98/32";
              };
            }
            { addressConfig = { Address = "fe80::100/64"; }; }
          ];
        };
        "potat0" = {
          matchConfig = { Name = "wg_potat0"; };
          networkConfig = {
            DHCP = "no";
            IPv6AcceptRA = false;
            IPForward = "yes";
            KeepConfiguration = "yes";
          };
          addresses = [
            {
              addressConfig = {
                Address = "172.22.240.97/32";
                Peer = "172.23.246.1/32";
              };
            }
            { addressConfig = { Address = "fe80::100/64"; }; }
          ];
        };
        "freeman" = {
          matchConfig = { Name = "wg_freeman"; };
          networkConfig = {
            DHCP = "no";
            IPv6AcceptRA = false;
            IPForward = "yes";
            KeepConfiguration = "yes";
          };
          addresses = [
            {
              addressConfig = {
                Address = "172.22.240.97/32";
                Peer = "172.22.240.98/32";
              };
            }
            { addressConfig = { Address = "fe80::100/64"; }; }
          ];
          routingPolicyRules = [
            {
              routingPolicyRuleConfig = {
                Table = 10;
                IncomingInterface = "eth1";
                Family = "both";
              };
            }
            {
              routingPolicyRuleConfig = {
                Table = 20;
                OutgoingInterface = "eth1";
              };
            }
            {
              routingPolicyRuleConfig = {
                Table = 30;
                From = "192.168.1.1";
                To = "192.168.1.2";
                SourcePort = 666;
                DestinationPort = 667;
              };
            }
            {
              routingPolicyRuleConfig = {
                Table = 40;
                IPProtocol = "tcp";
                InvertRule = true;
              };
            }
            {
              routingPolicyRuleConfig = {
                Table = 50;
                IncomingInterface = "eth1";
                Family = "ipv4";
              };
            }
          ];
        };
      };
    };
    timers.dn42-roa = {
      description = "Trigger a ROA table update";

      timerConfig = {
        OnBootSec = "5m";
        OnUnitInactiveSec = "1h";
        Unit = "dn42-roa.service";
      };

      wantedBy = [ "timers.target" ];
      before = [ "bird2.service" ];
    };

    services = {
      dn42-roa = {
        after = [ "network.target" ];
        description = "DN42 ROA Updated";
        unitConfig = { Type = "one-shot"; };
        serviceConfig = { ExecStart = "${script}/bin/update-roa"; };
      };
    };
  };
  services = {

    postfix = {
      inherit domain;
      enable = true;
    };

    openssh.enable = true;

    bird2 = {
      enable = true;
      checkConfig = false;
      config = ''
        ################################################
        #               Variable header                #
        ################################################

        define OWNAS =  4242422616;
        define OWNIP =  172.22.240.97;
        define OWNIPv6 = fd48:4b4:f3::1;
        define OWNNET = 172.22.240.96/27;
        define OWNNETv6 = fd48:4b4:f3::/48;
        define OWNNETSET = [172.22.240.96/27+];
        define OWNNETSETv6 = [fd48:4b4:f3::/48+];

        ################################################
        #                 Header end                   #
        ################################################

        router id OWNIP;

        protocol device {
            scan time 10;
        }

        /*
         *  Utility functions
         */

        function is_self_net() {
          return net ~ OWNNETSET;
        }

        function is_self_net_v6() {
          return net ~ OWNNETSETv6;
        }

        function is_valid_network() {
          return net ~ [
            172.20.0.0/14{21,29}, # dn42
            172.20.0.0/24{28,32}, # dn42 Anycast
            172.21.0.0/24{28,32}, # dn42 Anycast
            172.22.0.0/24{28,32}, # dn42 Anycast
            172.23.0.0/24{28,32}, # dn42 Anycast
            172.31.0.0/16+,       # ChaosVPN
            10.100.0.0/14+,       # ChaosVPN
            10.127.0.0/16{16,32}, # neonetwork
            10.0.0.0/8{15,24}     # Freifunk.net
          ];
        }

        roa4 table dn42_roa;
        roa6 table dn42_roa_v6;

        function is_valid_network_v6() {
          return net ~ [
            fd00::/8{44,64} # ULA address space as per RFC 4193
          ];
        }

        protocol kernel {
            scan time 20;

            ipv6 {
                import none;
                export filter {
                    if source = RTS_STATIC then reject;
                    krt_prefsrc = OWNIPv6;
                    accept;
                };
            };
        };

        protocol kernel {
            scan time 20;

            ipv4 {
                import none;
                export filter {
                    if source = RTS_STATIC then reject;
                    krt_prefsrc = OWNIP;
                    accept;
                };
            };
        }

        protocol static {
            route OWNNET reject;

            ipv4 {
                import all;
                export none;
            };
        }

        protocol static {
            route OWNNETv6 reject;

            ipv6 {
                import all;
                export none;
            };
        }

        template bgp dnpeers {
            local as OWNAS;
            path metric 1;

            ipv4 {
                import filter {
                  if is_valid_network() && !is_self_net() then {
                    if (roa_check(dn42_roa, net, bgp_path.last) != ROA_VALID) then {
                      print "[dn42] ROA check failed for ", net, " ASN ", bgp_path.last;
                      reject;
                    } else accept;
                  } else reject;
                };

                export filter { if is_valid_network() && source ~ [RTS_STATIC, RTS_BGP] then accept; else reject; };
                import limit 1000 action block;
            };

            ipv6 {   
                import filter {
                  if is_valid_network_v6() && !is_self_net_v6() then {
                    if (roa_check(dn42_roa_v6, net, bgp_path.last) != ROA_VALID) then {
                      print "[dn42] ROA check failed for ", net, " ASN ", bgp_path.last;
                      reject;
                    } else accept;
                  } else reject;
                };
                export filter { if is_valid_network_v6() && source ~ [RTS_STATIC, RTS_BGP] then accept; else reject; };
                import limit 1000 action block; 
            };
        }
        protocol static {
            roa4 { table dn42_roa; };
            include "/etc/bird/roa_dn42.conf";
        };

        protocol static {
            roa6 { table dn42_roa_v6; };
            include "/etc/bird/roa_dn42_v6.conf";
        };                              

        protocol bgp dn42_theresa_v6 from dnpeers {
          neighbor fe80::3396%wg_theresa as 4242423396;

          ipv4 {
            extended next hop on;
          };

          direct;
        }
        protocol bgp potat0 from dnpeers {
              neighbor 172.23.246.1 as 4242421816;
        }

        protocol bgp potat0_v6 from dnpeers {
             neighbor fe80::1816%wg_potat0 as 4242421816;
        }
      '';
    };
    bird-lg = {
      user = "root";
      group = "root";
      package = pkgs.symlinkJoin {
        name = "bird-lg";
        paths = with xddxdd; [ bird-lg-go bird-lgproxy-go ];
      };
      proxy = {
        enable = true;
        birdSocket = "/var/run/bird/bird.ctl";
        listenAddress = "0.0.0.0:8000";
      };
      frontend = {
        inherit domain;
        enable = true;
        netSpecificMode = "dn42";
        servers = [ "sg1" ];
      };
    };
    nginx = {
      enable = true;
      virtualHosts."default" = {
        addSSL = true;
        sslCertificateKey = ../ssl/PRIVATEKEYNOPASS.key;
        sslCertificate = ../ssl/SERVER.cert;
        # enableACME = true;
        locations."/" = {
          proxyPass = "http://127.0.0.1:5000";
          proxyWebsockets = true;
        };
      };
    };
  };
}
