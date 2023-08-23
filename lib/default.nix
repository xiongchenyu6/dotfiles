_: {
  # path -> [path]
  ls = dir:
    builtins.map (f: (dir + "/${f}"))
    (builtins.attrNames (builtins.readDir dir));

  bird2-inner-config = OWNIP: OWNIPv6: ''
    log stderr all;

    ################################################
    #               Variable header                #
    ################################################

    define OWNAS =  4242422616;
    define OWNIP = ${OWNIP};
    define OWNIPv6 = ${OWNIPv6};
    define OWNNET = 172.22.240.96/27;
    define OWNNETv6 = fd48:4b4:f3::/48;
    define OWNNETSET = [172.22.240.96/27+];
    define OWNNETSETv6 = [fd48:4b4:f3::/48+];

    ################################################
    #                 Header end                   #
    ################################################

    router id OWNIP;

    debug protocols all;
    log syslog { debug, trace, info, remote, warning, error, auth, fatal, bug };

    timeformat protocol     iso long; # neccessary for monitoring

    /*
    *  Utility functions
    */
        protocol device {
        scan time 10;
        }

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

    protocol babel int_babel {
    interface "wg_mail" {
      type wired;
    };
    ipv4 {
            export where (source = RTS_DEVICE) || (source = RTS_BABEL);
    };
    ipv6 {
            export where (source = RTS_DEVICE) || (source = RTS_BABEL);
    };
    };

    #protocol bgp ibgp_digital  {
    #  local as OWNAS;
    #  neighbor fe80::100%wg_mail as OWNAS;
    #  direct;
    #  ipv4 {
    #      next hop self;
    #      # Optional cost, e.g. based off latency
    #      cost 50;

    #      import all;
    #      export all;
    #  };
    #  ipv6 {
    #      next hop self;
    #      cost 50;
    #      import all;
    #      export all;
    #  };
    #}


  '';
}
