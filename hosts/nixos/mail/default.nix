{ modulesPath, suites, profiles, config, ... }: {
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
    profiles.server-apps.acme
    profiles.server-apps.admin.kerberos
    profiles.server-apps.admin.openldap
    profiles.server-apps.admin.sasl
    profiles.server-apps.admin.sssd
    profiles.server-apps.bgp.bird-border
    profiles.server-apps.dns.bind
    profiles.server-apps.log.loki
    profiles.server-apps.mail.postfix
    profiles.server-apps.mail.dovecot2
    profiles.server-apps.mail.alps
    profiles.server-apps.monitor.endlessh-go
    profiles.server-apps.monitor.grafana
    profiles.server-apps.monitor.prometheus
    profiles.server-apps.proxy.nginx
    profiles.server-apps.atuin
    profiles.server-apps.webapps.keycloak
    profiles.server-apps.oci-arm-host-capacity
    profiles.server-pkgs.nixos
    profiles.users.root.nixos
    profiles.users."freeman.xiong"
  ] ++ suites.server-base;

  boot.loader.grub.device = "/dev/vda";
  boot.initrd.availableKernelModules = [ "ata_piix" "uhci_hcd" "xen_blkfront" ];
  boot.initrd.kernelModules = [ "nvme" ];

  fileSystems."/" = {
    device = "/dev/vda2";
    fsType = "ext4";
  };

  zramSwap.enable = true;

  boot = {
    #isContainer = true;
    cleanTmpDir = true;
    kernel = {
      sysctl = {
        "net.ipv4.ip_forward" = 1;
        "net.ipv6.conf.all.forwarding" = 1;
        "net.ipv6.conf.default.forwarding" = 1;
        "net.ipv4.conf.default.rp_filter" = 0;
        "net.ipv4.conf.all.rp_filter" = 0;
      };
    };
  };

  sops.secrets."wireguard/mail" = { };
  # sops.age.keyFile = "/var/lib/sops-nix/key.txt";
  # sops.age.generateKey = true;

  networking = {
    nat = {
      enable = true;
      enableIPv6 = true;
      externalInterface = "ens5";
      internalInterfaces = [ "wg_office" ];
    };

    firewall = {
      enable = true;
      allowedTCPPorts = [
        25 # SMTP
        53
        80 # ui
        88 # kerberos
        179
        389
        443
        464 # kerberos change password
        465 # smpts
        636 # ldaps
        993 # imaps
        8000
        8888
      ];
      allowedUDPPorts = [
        53 # dns
        80
        88 # kerberos
        179 # bird2
        389 # ldap
        636
        22616
        22617
        23396
        21816
        33434
      ];
    };
    sits = {
      he-ipv6 = {
        local = "10.0.8.10";
        remote = "216.218.221.42";
        ttl = 255;
        dev = "ens5";
      };
    };
    interfaces = {
      he-ipv6 = {
        ipv6 = {
          routes = [{
            address = "::";
            prefixLength = 0;
          }];
          addresses = [{
            address = "2001:470:35:606::2";
            prefixLength = 64;
          }];
        };
      };
    };
    wg-quick = {
      interfaces = {
        wg_office = {
          privateKeyFile = config.sops.secrets."wireguard/mail".path;
          address = [ "172.22.240.97/27" "fe80::100/64" "fd48:4b4:f3::1/48" ];
          listenPort = 22616;
          table = "off";
          peers = [{
            publicKey = profiles.share.hosts-dict.office.wg.public-key;
            allowedIPs =
              [ "172.22.240.98/32" "fe80::101/128" "fd48:4b4:f3::2/128" ];
          }];
        };
        wg_theresa = {
          privateKeyFile = config.sops.secrets."wireguard/mail".path;
          address = [ "172.22.240.97/27" "fe80::100/64" ];
          listenPort = 23396;
          table = "off";
          peers = [{
            endpoint = "cn2.dn42.theresa.cafe:22616";
            publicKey = "MqKkzCwYfOg8Fc/pRRctLW3jS72ACBDQr8ZF10sZ614=";
            allowedIPs = [
              "10.0.0.0/8"
              "172.20.0.0/14"
              "172.31.0.0/16"
              "fd00::/8"
              "fe80::/64"
            ];
          }];
        };
        wg_potat0 = {
          privateKeyFile = config.sops.secrets."wireguard/mail".path;
          address = [ "172.22.240.97/27" "fe80::100/64" ];
          listenPort = 21816;
          table = "off";
          peers = [{
            endpoint = "us1.dn42.potat0.cc:22616";
            publicKey = "LUwqKS6QrCPv510Pwt1eAIiHACYDsbMjrkrbGTJfviU=";
            allowedIPs = [
              "10.0.0.0/8"
              "172.20.0.0/14"
              "172.31.0.0/16"
              "fd00::/8"
              "fe80::/64"
            ];
          }];
        };
        wg_tech9 = {
          privateKeyFile = config.sops.secrets."wireguard/mail".path;
          address = [ "172.22.240.97/27" "fe80::100/64" ];
          listenPort = 21588;
          table = "off";
          peers = [{
            endpoint = "sg-sin01.dn42.tech9.io:52507";
            publicKey = "4qLIJ9zpc/Xgvy+uo90rGso75cSrT2F5tBEv+6aqDkY=";
            allowedIPs = [
              "10.0.0.0/8"
              "172.20.0.0/14"
              "172.31.0.0/16"
              "fd00::/8"
              "fe80::/64"
            ];
          }];
        };
      };
    };
  };
  services = {
    prometheus.exporters = {
      postgres = {
        enable = true;
        port = 9009;
      };

      wireguard = {
        enable = true;
        port = 9010;
      };
    };
  };

}
