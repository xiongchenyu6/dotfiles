{ config, ... }: {
  services = {
    bird-lg = {
      # package = pkgs.symlinkJoin {
      #   name = "bird-lg";
      #   paths = with pkgs; [ bird-lg-go bird-lgproxy-go ];
      # };
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
        protocolFilter = [ "bgp" "babel" "static" ];
        whois = "whois.burble.dn42";
        # titleBrand = "Freeman dn42 bird-lg";
        dnsInterface = "asn.lantian.dn42";
        navbar = {
          # brand = "Freeman dn42 bird-lg";
        };
      };
    };
    nginx = {
      virtualHosts = {
        bird-lg = {
          serverName = "bird-lg.inner.${config.networking.domain}";
          forceSSL = true;
          acmeRoot = null;
          useACMEHost = "inner.${config.networking.domain}";
          kTLS = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:5000";
            proxyWebsockets = true;
          };
        };
      };
    };
  };
}
