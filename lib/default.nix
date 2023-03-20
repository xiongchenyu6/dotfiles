_: {
  # path -> [path]
  ls = dir:
    builtins.map (f: (dir + "/${f}"))
    (builtins.attrNames (builtins.readDir dir));

  bird2-inner-config = OWNIP: OWNIPv6: ''
    log syslog all;
    # log "/var/log/bird.log" all;
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
    protocol kernel {
        scan time 20;
        learn;

        ipv6 {
            import all;
            # import none;
            # export filter {
            #     if source = RTS_STATIC then reject;
            #     krt_prefsrc = OWNIPv6;
            #     accept;
            # };
        };
    };

    protocol kernel {
        scan time 20;
        learn;

        ipv4 {
            import all;
            # import none;
            # export filter {
            #     if source = RTS_STATIC then reject;
            #     krt_prefsrc = OWNIP;
            #     accept;
            # };
        };
    };

    protocol device {
       scan time 10;
    };

    function is_self_net() {
        return net ~ [
            172.22.240.96/27+
        ];
    };

    function is_self_net_v6() {
        return net ~ [
            fd48:4b4:f3::/48+
        ];
    };

    protocol babel int_babel{
        ipv4 {
            import all;
            import all;
        };
        ipv6 {
            import all;
            import all;
        };
        interface "wg_mail" {
             type wired;
        };
        interface "ens3" {
             type wired;
        };
        interface "ens4" {
             type wired;
        };
    };
    protocol direct {
        ipv4;
        ipv6;
        interface "lo";
    };
  '';
}
