with builtins; rec {

  users = [
    {
      gn = "freeman";
      sn = "xiong";
      gid = 1234;
      tel = 123434132;
      public-key =
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIABVd0cIdwKzf4yLoRXQwjaaVYPFv8ZfYvTUMOMTFJ/p freeman@nixos";
    }
    {
      gn = "user";
      sn = "3";
      gid = 1233;
      tel = 123423413;
      public-key =
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIABVd0cIdwKzf4yLoRXQwjaaVYPFv8ZfYvTUMOMTFJ/p freeman@nixos";
    }
    {
      gn = "user";
      sn = "5";
      gid = 1234;
      tel = 1234423432;
      public-key =
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIABVd0cIdwKzf4yLoRXQwjaaVYPFv8ZfYvTUMOMTFJ/p freeman@nixos";
    }
  ];

  group-dict = [
    {
      name = "test1";
      users = [ "user.3" ];
    }
    {
      name = "test2";
      users = [ "user.5" ];
    }
    {
      name = "test3";
      users = [ "user.3" "user.5" ];
    }
    {
      name = "podman";
      users = [ "freeman.xiong" "user.3" "user.5" ];
    }
    {
      name = "developer";
      id = 1233;
    }
    {
      name = "owner";
      id = 1234;
    }
  ];

  hosts = [
    {
      host = "game";
      role = "internal";
      network = "dn42";
      public-key =
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK9lhSffZNM3UYm884iQc/XmWL+g5fnePXUh4mPFkuNy root@nixos";
      wg = { public-key = "pvInS7gtW3H8JN3wzkdK8HM1qqFE/LpRXP36+z5EGEc="; };
    }
    {
      host = "digital";
      role = "border";
      endpoint = "digital.freeman.engineer";
      network = "dn42";
      wg = { public-key = "aTfi+nYiVY2dkqS0Z3OqKpQeXIHamf28EQYhwaurhBI="; };
    }
    {
      host = "office";
      role = "internal";
      network = "dn42";
      public-key =
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK9lhSffZNM3UYm884iQc/XmWL+g5fnePXUh4mPFkuNy root@nixos";
      wg = { public-key = "trmPW+CV8BbXfDMbe7I7IFwRh5ke8vpbDlgisSoH6ng="; };
    }
    {
      host = "mail";
      network = "dn42";
      role = "vpn";
      endpoint = "mail.freeman.engineer";
      public-key =
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKcBsPZi+OYEL/RCSGZMr82x0UGUaghP3AGl6M57ssjn";
      wg = {
        port = 22616;
        public-key = "9TXI2YQ0cdhW3xBhxzuHpPuISR7k2NwTjZ2Sq/lwoE0=";
      };
      connect = [rec {
        network = "dn42";
        hosts = [
          {
            host = "theresa";
            listen = 23396;
          }
          {
            host = "potat0";
            listen = 21816;
          }
          {
            host = "tech9";
            listen = 21588;
          }
        ];
        connect-dict = listToAttrs (map (h: {
          name = h.host;
          value = h;
        }) hosts);

        address = [ "172.22.240.97/27" "fe80::100/64" ];
      }];
    }
    {
      host = "theresa";
      role = "external";
      network = "dn42";
      endpoint = "cn2.dn42.theresa.cafe";
      wg = {
        port = 22616;
        public-key = "MqKkzCwYfOg8Fc/pRRctLW3jS72ACBDQr8ZF10sZ614=";
      };
    }
    {
      host = "potat0";
      role = "external";
      network = "dn42";
      endpoint = "us1.dn42.potat0.cc";
      wg = {
        port = 22616;
        public-key = "LUwqKS6QrCPv510Pwt1eAIiHACYDsbMjrkrbGTJfviU=";
      };
    }
    {
      host = "tech9";
      role = "external";
      network = "dn42";
      endpoint = "sg-sin01.dn42.tech9.io";
      wg = {
        port = 52507;
        public-key = "4qLIJ9zpc/Xgvy+uo90rGso75cSrT2F5tBEv+6aqDkY=";
      };
    }
    {
      host = "tronlink";
      role = "vpn";
      network = "tronlink";
      endpoint = "vpn.trontech.link";
      wg = {
        port = 22617;
        public-key = "MEjaDVdOqGEOjO6m23yHq5ZCzeZC0Id8jxCKEUPdxhw=";
      };
    }
  ];

  networks = [rec {
    name = "dn42";
    OWNAS = "4242422616";
    OWNNET = "172.22.240.96/27";
    OWNNETv6 = "fd48:4b4:f3::/48";
    allowedIPs = [
      "10.0.0.0/8"
      "172.20.0.0/14"
      "172.31.0.0/16"
      "fd00::/8"
      "fe80::/10"
      "fd48:4b4:f3::/48"
    ];
    assignedIPs = [
      rec {
        host = "mail";
        ipv4 = "172.22.240.97/32";
        ipv6linklocal = "fe80::100/128";
        ipv6global = "fd48:4b4:f3::1/128";
        address = [ ipv4 ipv6linklocal ipv6global ];
      }
      rec {
        host = "office";
        ipv4 = "172.22.240.98/32";
        ipv6linklocal = "fe80::101/128";
        ipv6global = "fd48:4b4:f3::2/128";
        address = [ ipv4 ipv6linklocal ipv6global ];
      }
      rec {
        host = "game";
        ipv4 = "172.22.240.99/32";
        ipv6linklocal = "fe80::102/128";
        ipv6global = "fd48:4b4:f3::3/128";
        address = [ ipv4 ipv6linklocal ipv6global ];
      }
    ];
    assignedIPs-dict = listToAttrs (map (h: {
      name = h.host;
      value = h;
    }) assignedIPs);

    externalInterface = [
      {
        host = "theresa";
        ipv6linklocal = "fe80::3396";
        as = "4242423396";
      }
      {
        host = "potat0";
        ipv6linklocal = "fe80::1816";
        as = "4242421816";
      }
      {
        host = "tech9";
        ipv6linklocal = "fe80::1588";
        as = "4242421588";
      }
    ];
    externalInterface-dict = listToAttrs (map (h: {
      name = h.host;
      value = h;
    }) externalInterface);
  }
  # rec {
  #   name = "tronlink";
  #   allowedIPs = [
  #     "172.64.224.1/24"
  #     "fe80::101/64"
  #     "172.32.0.0/16"
  #     "18.218.96.133/32"
  #     "13.212.2.33"
  #   ];
  #   assignedIPs = [{
  #     host = "office";
  #     address = [ "172.64.224.3/24" "fe80::103/64" ];
  #   }];
  #   assignedIPs-dict = listToAttrs (map (h: {
  #     name = h.host;
  #     value = h;
  #   }) assignedIPs);
  # }
    ];

  networks-dict = listToAttrs (map (h: {
    inherit (h) name;
    value = h;
  }) networks);

  root-cas = [
    {
      ca = "dn42";
      cert = ''
        -----BEGIN CERTIFICATE-----
        MIID8DCCAtigAwIBAgIFIBYBAAAwDQYJKoZIhvcNAQELBQAwYjELMAkGA1UEBhMC
        WEQxDTALBgNVBAoMBGRuNDIxIzAhBgNVBAsMGmRuNDIgQ2VydGlmaWNhdGUgQXV0
        aG9yaXR5MR8wHQYDVQQDDBZkbjQyIFJvb3QgQXV0aG9yaXR5IENBMCAXDTE2MDEx
        NjAwMTIwNFoYDzIwMzAxMjMxMjM1OTU5WjBiMQswCQYDVQQGEwJYRDENMAsGA1UE
        CgwEZG40MjEjMCEGA1UECwwaZG40MiBDZXJ0aWZpY2F0ZSBBdXRob3JpdHkxHzAd
        BgNVBAMMFmRuNDIgUm9vdCBBdXRob3JpdHkgQ0EwggEiMA0GCSqGSIb3DQEBAQUA
        A4IBDwAwggEKAoIBAQDBGRDeAYYR8YIMsNTl/5rI46r0AAiCwM9/BXohl8G1i6PR
        VO76BA931VyYS9mIGMEXEJLlJPrvYetdexHlvrqJ8mDJO4IFOnRUYCNmGtjNKHvx
        6lUlmowEoP+dSFRMnbwtoN9xrmRHDed1BfTFAirSDL6jY1RiK60p62oIpF6o6/FS
        FE7RXUEv0xm65II2etGj8oT2B7L2DDDb23bu6RQFx491tz/V1TVW0JJE3yYeAPqu
        y3rJUGddafj5/SWnHdtAsUK8RVfhyRxCummAHuolmRKfbyOj0i5KzRXkfEn50cDw
        GQwVUM6mUbuqFrKC7PRhRIwc3WVgBHewTZlnF/sJAgMBAAGjgaowgacwDgYDVR0P
        AQH/BAQDAgEGMA8GA1UdEwEB/wQFMAMBAf8wHQYDVR0OBBYEFFR2iLLAtTDQ/E/J
        bTv5jFURrBUVMB8GA1UdIwQYMBaAFFR2iLLAtTDQ/E/JbTv5jFURrBUVMEQGA1Ud
        HgQ9MDugOTAHggUuZG40MjAKhwisFAAA//wAADAihyD9QgAAAAAAAAAAAAAAAAAA
        //8AAAAAAAAAAAAAAAAAADANBgkqhkiG9w0BAQsFAAOCAQEAXKQ7QaCBaeJxmU11
        S1ogDSrZ7Oq8jU+wbPMuQRqgdfPefjrgp7nbzfUW5GrL58wqj+5/FAqltflmSIHl
        aB4MpqM8pyvjlc/jYxUNFglj2WYxO0IufBrlKI5ePZ4omUjpR4YR4gQpYCuWlZmu
        P6v/P0WrfgdFTk0LGEA9OwKcTqkPpcI/SjB3rmZcs42yQWvimAF94GtScE09uKlI
        9QLS2UBmtl5EJRFVrDEC12dyamq8dDRfddyaT4MoQOAq3D9BQ1pHByu3pz/QFaJC
        1zAi8vbktPY7OMprTOc8pHDL3q8KFP8jJcoEzZ5Jw0vkCrULhLXvtFtjB0djzVxQ
        C0IKqQ==
        -----END CERTIFICATE-----
      '';

    }
    {
      ca = "digitcert-global";
      cert = ''
        -----BEGIN CERTIFICATE-----
        MIIDrzCCApegAwIBAgIQCDvgVpBCRrGhdWrJWZHHSjANBgkqhkiG9w0BAQUFADBh
        MQswCQYDVQQGEwJVUzEVMBMGA1UEChMMRGlnaUNlcnQgSW5jMRkwFwYDVQQLExB3
        d3cuZGlnaWNlcnQuY29tMSAwHgYDVQQDExdEaWdpQ2VydCBHbG9iYWwgUm9vdCBD
        QTAeFw0wNjExMTAwMDAwMDBaFw0zMTExMTAwMDAwMDBaMGExCzAJBgNVBAYTAlVT
        MRUwEwYDVQQKEwxEaWdpQ2VydCBJbmMxGTAXBgNVBAsTEHd3dy5kaWdpY2VydC5j
        b20xIDAeBgNVBAMTF0RpZ2lDZXJ0IEdsb2JhbCBSb290IENBMIIBIjANBgkqhkiG
        9w0BAQEFAAOCAQ8AMIIBCgKCAQEA4jvhEXLeqKTTo1eqUKKPC3eQyaKl7hLOllsB
        CSDMAZOnTjC3U/dDxGkAV53ijSLdhwZAAIEJzs4bg7/fzTtxRuLWZscFs3YnFo97
        nh6Vfe63SKMI2tavegw5BmV/Sl0fvBf4q77uKNd0f3p4mVmFaG5cIzJLv07A6Fpt
        43C/dxC//AH2hdmoRBBYMql1GNXRor5H4idq9Joz+EkIYIvUX7Q6hL+hqkpMfT7P
        T19sdl6gSzeRntwi5m3OFBqOasv+zbMUZBfHWymeMr/y7vrTC0LUq7dBMtoM1O/4
        gdW7jVg/tRvoSSiicNoxBN33shbyTApOB6jtSj1etX+jkMOvJwIDAQABo2MwYTAO
        BgNVHQ8BAf8EBAMCAYYwDwYDVR0TAQH/BAUwAwEB/zAdBgNVHQ4EFgQUA95QNVbR
        TLtm8KPiGxvDl7I90VUwHwYDVR0jBBgwFoAUA95QNVbRTLtm8KPiGxvDl7I90VUw
        DQYJKoZIhvcNAQEFBQADggEBAMucN6pIExIK+t1EnE9SsPTfrgT1eXkIoyQY/Esr
        hMAtudXH/vTBH1jLuG2cenTnmCmrEbXjcKChzUyImZOMkXDiqw8cvpOp/2PV5Adg
        06O/nVsJ8dWO41P0jmP6P6fbtGbfYmbW0W5BjfIttep3Sp+dWOIrWcBAI+0tKIJF
        PnlUkiaY4IBIqDfv8NZ5YBberOgOzW6sRBc4L0na4UU+Krk2U886UAb3LujEV0ls
        YSEY1QSteDwsOoBrp+uvFRTp2InBuThs4pFsiv9kuXclVzDAGySj4dzp30d8tbQk
        CAUw7C29C79Fv1C5qfPrmAESrciIxpg0X40KPMbp1ZWVbd4=
        -----END CERTIFICATE-----
      '';
    }
  ];

  users-dict = listToAttrs (map (u: {
    name = "${u.gn}.${u.sn}";
    value = u;
  }) users);

  hosts-dict = listToAttrs (map (h: {
    name = h.host;
    value = h;
  }) hosts);

  root-cas-dict = listToAttrs (map (c: {
    name = c.ca;
    value = c;
  }) root-cas);
}
