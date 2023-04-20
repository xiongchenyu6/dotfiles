{
  imports = [ ./birg-lg.nix ];

  services = {
    prometheus.exporters = {
      bird = {
        enable = true;
        port = 9003;
      };
    };
    bird2 = {
      enable = true;
      checkConfig = false;
      config = ''
        log syslog all;
        # log "/var/log/bird.log" all;
        log stderr all;
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
        learn;
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
        learn;

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

        protocol bgp potat0_v6 from dnpeers {
        neighbor fe80::1816%wg_potat0 as 4242421816;
        ipv4 {
        extended next hop on;
        };

        direct;
        }

        protocol bgp tech9_v6 from dnpeers {
        neighbor fe80::1588%wg_tech9 as 4242421588;
        ipv4 {
        extended next hop on;
        };

        direct;
        }

        protocol bgp ibgp_digital  {

          local as OWNAS;
          neighbor fe80::103%wg_digital as OWNAS;
          direct;

          ipv4 {
              next hop self;
              # Optional cost, e.g. based off latency
              cost 50;

              import all;
              export all;
          };

          ipv6 {
              next hop self;
              cost 50;

              import all;
              export all;
          };
        }

        # protocol babel int_babel {
        # ipv4 {
        # import all;
        # import all;
        # };
        # ipv6 {
        # import all;
        # import all;
        # };
        # interface "wg_office" {
        #   # Note: Babel's cost metric is slightly different from BGP and OSPF.
        #   # rxcost specifies the cost for the neighbour to send traffic to us,
        #   # not the cost to send TO that neighbour. Of course, this won't have
        #   # any impact if you keep costs consistent on both ends.
        #   rxcost 123;
        #   type wired;
        # };
        # interface "wg_game" {
        #   # Note: Babel's cost metric is slightly different from BGP and OSPF.
        #   # rxcost specifies the cost for the neighbour to send traffic to us,
        #   # not the cost to send TO that neighbour. Of course, this won't have
        #   # any impact if you keep costs consistent on both ends.
        #   rxcost 122;
        #   type wired;
        # };
        # interface "wg_digital" {
        #   # Note: Babel's cost metric is slightly different from BGP and OSPF.
        #   # rxcost specifies the cost for the neighbour to send traffic to us,
        #   # not the cost to send TO that neighbour. Of course, this won't have
        #   # any impact if you keep costs consistent on both ends.
        #   rxcost 122;
        #   type wired;
        # };
        # };

        # protocol direct {
        # ipv4;
        # ipv6;
        # interface "lo";
        # };
      '';
    };
  };
}
