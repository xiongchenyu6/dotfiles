{ modulesPath, profiles, config, lib, pkgs, ... }:
let
  # generate via openvpn --genkey --secret openvpn-laptop.key
  vpn-dev = "tun0";
  port = 1194;

in {
  imports = [
    (modulesPath + "/profiles/qemu-guest.nix")
    ../../../profiles/server/apps/acme
    # ../../../profiles/server/apps/admin/openldap.nix
    # ../../../profiles/server/apps/adm          in/sasl.nix
    # ../../../profiles/server/apps/admin/sssd.nix
    ../../../profiles/server/apps/bgp/bird-border.nix
    # ../../../profiles/server/apps/dns/bind.nix
    ../../../profiles/core/nixos.nix
    # ../../../profiles/server-apps/log/loki.nix
    # ../../../profiles/server/apps/mail/postfix.nix
    # ../../../profiles/server/apps/mail/dovecot2.nix
    # ../../../profiles/server/apps/mail/alps.nix
    # ../../../profiles/server-apps/monitor/endlessh-go.nix
    # ../../../profiles/server-apps/monitor/prometheus.nix
    ../../../profiles/server/apps/proxy/nginx.nix
    # ../../../profiles/server-apps/atuin.nix
    # ../../../profiles/server-apps/webapps/keycloak.nix
    #    ../../../profiles/server/apps/oci-arm-host-capacity.nix
    ../../../profiles/server/components
    ../../../users/root/nixos.nix
    ../../../users/freeman.xiong
    ../../../profiles/sops.nix
    ../../../profiles/common/components
    ../../../profiles/common/apps/dn42
    ../../../profiles/common/apps/bird-inner.nix
    ../../../profiles/server/components
    ../../../profiles/server/apps/log/promtail.nix
    # ../../../profiles/server/apps/admin/sssd.nix
    ../../../profiles/common/components/datadog-agent.nix
  ];

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
    tmp.cleanOnBoot = true;
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

  sops.secrets."wireguard/mail" = { };
  # sops.age.keyFile = "/var/lib/sops-nix/key.txt";
  # sops.age.generateKey = true;

  networking = let
    file-path = builtins.split "/" (toString ./.);
    hostName = lib.last file-path;
  in {
    inherit hostName;

    nat = {
      enable = true;
      enableIPv6 = true;
      externalInterface = "ens5";
      internalInterfaces = [ "wg_game" "wg_office" vpn-dev ];
    };

    firewall = {
      enable = true;
      trustedInterfaces = [ vpn-dev ];
      allowedTCPPorts = [
        25 # SMTP
        53
        80 # ui
        88 # kerberos
        89
        179
        389
        443
        464 # kerberos change password
        465 # smpts
        636 # ldaps
        993 # imaps
        6695
        8888
        10086
        18000
      ];
      allowedUDPPorts = [
        port
        53 # dns
        80
        89
        88 # kerberos
        179 # bird2
        389 # ldap
        636
        5353 # mdns
        6696
        22616
        22617
        22618
        23396
        21816
        33434
      ];

      interfaces.wg_mail.allowedTCPPorts = [ 2222 ];
      interfaces.wg_mail.allowedUDPPorts = [ 2222 ];
      interfaces.wg_game.allowedTCPPorts = [ 2222 ];
      interfaces.wg_game.allowedUDPPorts = [ 2222 ];

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
      interfaces = let
        privateKeyFile = config.sops.secrets."wireguard/mail".path;
        # address = [ "fe80::100/64" "fd48:4b4:f3::1/48" "172.22.240.97/27" ];
        address = [ "fe80::100/64" ];
        table = "off";
        allowedIPs = [
          "10.0.0.0/8"
          "172.20.0.0/14"
          "172.31.0.0/16"
          "fd00::/8"
          "fe80::/64"
          "fd48:4b4:f3::/48"
          "ff02::1:6/128"
          "224.0.0.251/32" # avahi
          "ff02::fb/128" # avahi
        ];
      in {
        wg_digital = {
          inherit privateKeyFile address table;
          listenPort = 22618;
          peers = [{
            endpoint = "178.128.82.145:22616";
            publicKey = profiles.share.hosts-dict.digital.wg.public-key;
            inherit allowedIPs;
          }];
        };

        wg_office = {
          inherit address privateKeyFile table;
          listenPort = 22616;
          postUp = ''
            ${pkgs.iproute2}/bin/ip addr add dev wg_office 172.22.240.97/32 peer 172.22.240.98/32
            ${pkgs.iproute2}/bin/ip addr add dev wg_office fd48:4b4:f3::1/128 peer fd48:4b4:f3::2/128
            ${pkgs.iproute2}/bin/ip link set multicast on dev wg_office
          '';
          peers = [{
            publicKey = profiles.share.hosts-dict.office.wg.public-key;
            inherit allowedIPs;
          }];
        };
        wg_game = {
          inherit address privateKeyFile table;
          listenPort = 22617;
          postUp = ''
            ${pkgs.iproute2}/bin/ip addr add dev wg_game 172.22.240.97/32 peer 172.22.240.99/32
            ${pkgs.iproute2}/bin/ip addr add dev wg_game fd48:4b4:f3::1/128 peer fd48:4b4:f3::3/128
            ${pkgs.iproute2}/bin/ip link set multicast on dev wg_game
          '';

          peers = [{
            publicKey = profiles.share.hosts-dict.game.wg.public-key;
            inherit allowedIPs;
          }];
        };
        wg_theresa = {
          inherit privateKeyFile address table;
          listenPort = 23396;
          peers = [{
            endpoint = "cn2.dn42.theresa.cafe:22616";
            publicKey = "MqKkzCwYfOg8Fc/pRRctLW3jS72ACBDQr8ZF10sZ614=";
            inherit allowedIPs;
          }];
        };
        wg_potat0 = {
          inherit privateKeyFile address table;
          listenPort = 21816;
          peers = [{
            endpoint = "us1.dn42.potat0.cc:22616";
            publicKey = "LUwqKS6QrCPv510Pwt1eAIiHACYDsbMjrkrbGTJfviU=";
            inherit allowedIPs;
          }];
        };
        wg_tech9 = {
          inherit privateKeyFile address table;
          listenPort = 21588;
          peers = [{
            endpoint = "sg-sin01.dn42.tech9.io:52507";
            publicKey = "4qLIJ9zpc/Xgvy+uo90rGso75cSrT2F5tBEv+6aqDkY=";
            inherit allowedIPs;
          }];
        };
      };
    };
  };
  services = {
    # avahi = { allowInterfaces = [ "wg_office" ]; };
    frp = {
      enable = true;
      role = "server";
      settings = { bindPort = 7000; };

    };
    v2ray = {
      enable = true;
      config = {
        inbounds = [{
          port = 10086;
          protocol = "vmess";
          settings = {
            clients = [{
              id = builtins.readFile ../../../secrets/v2ray.password;
              alterId = 64;
            }];
          };
        }];
        outbounds = [{
          protocol = "freedom";
          settings = { };
        }];
      };
    };

    journald = {
      extraConfig = ''
        Storage=volatile
        RuntimeMaxUse=30M
      '';
    };
    datadog-agent = { tags = [ "env:outter" ]; };

    openssh = let
      file = pkgs.writeText "ca.pub"
        "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBEpEaKdRToEGqji1PLnZsP+AaScTYQbcdkLCYYPe+gYX5ILxcuHXL+O5GdHzs+LtC6csdvzzBQBaJEpT7pr/GsM=";
    in {
      openFirewall = true;
      hostKeys = lib.mkAfter [
        {
          bits = 4096;
          path = "/etc/ssh/ssh_host_rsa_key";
          type = "rsa";
        }
        {
          path = "/etc/ssh/ssh_host_ed25519_key";
          type = "ed25519";
        }
        {
          path = "/etc/ssh/ssh_host_ecdsa_key";
          type = "ecdsa-sha2-nistp256";
        }
      ];
      extraConfig = ''
        HostCertificate /etc/ssh/ssh_host_ecdsa_key-cert.pub
        TrustedUserCAKeys ${file}
      '';
    };
    # sshguard = { enable = true; };

    # babeld.enable = true;
    babeld.extraConfig = ''
      redistribute ip 172.20.0.0/14
      redistribute if ens5 deny
    '';

    babeld.interfaces = {
      wg_game = {
        hello-interval = 5;
        split-horizon = "auto";
        type = "wired";

      };
      wg_office = {
        hello-interval = 5;
        split-horizon = "auto";
        type = "wired";
      };
      wg_digital = {
        hello-interval = 5;
        split-horizon = "auto";
        type = "wired";
      };
    };

    # prometheus.exporters = {
    #   postgres = {
    #     enable = true;
    #     port = 9009;
    #   };

    #   wireguard = {
    #     enable = true;
    #     port = 9010;
    #   };
    # };
  };
  home-manager = {
    users = {
      "freeman.xiong" = { programs = { eza = { enable = true; }; }; };
    };
  };

}
