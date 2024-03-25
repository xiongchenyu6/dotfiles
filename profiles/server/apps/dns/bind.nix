{ config, pkgs, ... }: {
  systemd = {
    services = {
      dns-rfc2136-conf = {
        requiredBy =
          [ "acme-${config.networking.domain}.service" "bind.service" ];
        before =
          [ "acme-${config.networking.domain}.service" "bind.service" ];
        unitConfig = {
          ConditionPathExists = "!/var/lib/secrets/dnskeys.conf";
        };
        serviceConfig = {
          Type = "oneshot";
          UMask = 77;
        };
        path = [ pkgs.bind pkgs.gnused ];
        script = ''
          mkdir -p /var/lib/secrets
          chown named:root /var/lib/secrets
          tsig-keygen rfc2136key.inner.${config.networking.domain} > /var/lib/secrets/dnskeys.conf
          chown named:root /var/lib/secrets/dnskeys.conf

          secret=`sed -n -e 's/secret "\(.*\)";/\1/p' /var/lib/secrets/dnskeys.conf`

          chmod 700 /var/lib/secrets/dnskeys.conf

          # Copy the secret value from the dnskeys.conf, and put it in
          # RFC2136_TSIG_SECRET below

          cat > /var/lib/secrets/certs.secret << EOF
          RFC2136_NAMESERVER='127.0.0.1:53'
          RFC2136_TSIG_ALGORITHM='hmac-sha256.'
          RFC2136_TSIG_KEY='rfc2136key.inner.${config.networking.domain}'
          RFC2136_TSIG_SECRET="''${secret:1}"
          EOF
          chmod 400 /var/lib/secrets/certs.secret
        '';
      };
      reload-private-zone = {
        requiredBy = [ "bind.service" ];
        before = [ "bind.service" ];
        serviceConfig = {
          Type = "oneshot";
          UMask = 77;
          RemainAfterExit = true;
        };
        script = ''
          mkdir -p /var/db/bind/
          chown named:root /var/db/bind/
          cat > /var/db/bind/inner.${config.networking.domain} << EOF
          \$TTL 3600
          \$ORIGIN inner.autolife-robotics.tech.
          @         IN SOA    inner.autolife-robotics.tech. hostmaster.inner.autolife-robotics.tech. ( 1 3h 1h 1w 1d )
          @         IN NS     ns1.inner.autolife-robotics.tech.
          ns1       IN A      43.156.66.157
          *         IN A      43.156.66.157
          office    IN A      172.22.240.98
          game      IN A      172.22.240.99
          EOF
          chown named:named /var/db/bind/inner.${config.networking.domain}
        '';
      };
    };
  };

  services = {
    bind = {
      enable = true;
      extraConfig = ''
        include "/var/lib/secrets/dnskeys.conf";

        zone "dn42" {
          type forward;
          forwarders { 172.20.0.53; fd42:d42:d42:54::1; };
        };
        zone "20.172.in-addr.arpa" {
          type forward;
          forwarders { 172.20.0.53; fd42:d42:d42:54::1; };
        };
        zone "21.172.in-addr.arpa" {
          type forward;
          forwarders { 172.20.0.53; fd42:d42:d42:54::1; };
        };
        zone "22.172.in-addr.arpa" {
          type forward;
          forwarders { 172.20.0.53; fd42:d42:d42:54::1; };
        };
        zone "23.172.in-addr.arpa" {
          type forward;
          forwarders { 172.20.0.53; fd42:d42:d42:54::1; };
        };
        zone "10.in-addr.arpa" {
          type forward;
          forwarders { 172.20.0.53; fd42:d42:d42:54::1; };
        };
        zone "d.f.ip6.arpa" {
          type forward;
          forwarders { 172.20.0.53; fd42:d42:d42:54::1; };
        };
      '';

      cacheNetworks = [
        "0.0.0.0/0"
        "203.117.163.130/32"
        "127.0.0.0/24"
        "10.0.0.0/8"
        "172.20.0.0/14"
        "172.31.0.0/16"
        "fd00::/8"
        "fe80::/64"
      ];
      zones = [
        rec {
          name = "inner.${config.networking.domain}";
          master = true;
          file = "/var/db/bind/${name}";
          extraConfig =
            "allow-update { key rfc2136key.inner.${config.networking.domain}.; };";
        }
        rec {
          name = "157.66.156.43.in-addr.arpa";
          master = true;
          file = pkgs.writeText "157.66.156.43.in-addr.arpa" ''
            $TTL 86400
            $ORIGIN 157.66.156.43.in-addr.arpa.
            @     IN     SOA    dns1.example.com.     hostmaster.example.com. (
                                2001062501 ; serial
                                21600      ; refresh after 6 hours
                                3600       ; retry after 1 hour
                                604800     ; expire after 1 week
                                86400 )    ; minimum TTL of 1 day

                  IN     NS     dns1.example.com.
                  IN     PTR    ${config.networking.fqdn}.
          '';
        }
      ];
      extraOptions = ''
        empty-zones-enable no;
      '';
    };
  };
}
