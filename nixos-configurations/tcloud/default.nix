{
  modulesPath,
  inputs,
  lib,
  ezModules,
  config,
  pkgs,
  shares,
  ...
}:
let
  vpn-dev = "tun0";
  port = 1194;
in
{
  imports = with inputs; [
    ezModules.root
    ezModules."freeman.xiong"
    ezModules.core
    ezModules.server
    ezModules.acme
    ezModules.bird-border
    ezModules.dn42
    ezModules.bird-inner
    #ezModules.datadog-agent
    ezModules.sing-box
    srvos.nixosModules.server
    srvos.nixosModules.mixins-nginx
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
    ./hardware-configuration.nix
    {
      topology.self.interfaces.home = {
        type = "wireguard";
        addresses = [ "172.22.240.97/27" ];
      };
    }

  ];
  security = {
    acme = {
      defaults = {
        group = "nginx";
      };
    };
  };

  boot.initrd.kernelModules = [ "nvme" ];

  zramSwap.enable = true;

  boot = {
    kernelPackages = lib.mkForce pkgs.linuxPackages_latest;
    tmp.cleanOnBoot = true;
    kernel = {
      sysctl = {
        "net.ipv4.ip_forward" = 1;
      };
    };
  };

  sops.secrets."wireguard/tcloud" = { };
  sops.secrets."coturn/credentials" = {
    owner = "turnserver";
    group = "turnserver";
  };

  # Inject coturn user credentials from sops at runtime
  systemd.services.coturn.serviceConfig.ExecStartPre = lib.mkAfter [
    "+${pkgs.writeShellScript "coturn-inject-credentials" ''
      cred=$(cat ${config.sops.secrets."coturn/credentials".path})
      echo "user=$cred" >> /etc/turnserver.conf
    ''}"
  ];

  networking = {
    nat = {
      enable = true;
      enableIPv6 = true;
      externalInterface = "ens5";
      internalInterfaces = [
        "wg_game"
        "wg_office"
        vpn-dev
      ];
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
        179 # bgp
        389
        443
        464 # kerberos change password
        465 # smpts
        636 # ldaps
        993 # imaps
        6695
        7000 # frp
        8000
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
        179 # bird
        389 # ldap
        636
        5353 # mdns
        6696
        22616
        22617
        22618
        22619
        22620
        23396
        21816
        33434
        3478 # stun
      ];
      allowedUDPPortRanges = [
        {
          from = 49152;
          to = 65535;
        }
      ];

      interfaces.wg_office.allowedTCPPorts = [ 22 ];
      interfaces.wg_office.allowedUDPPorts = [ 22 ];
      interfaces.wg_game.allowedTCPPorts = [ 22 ];
      interfaces.wg_game.allowedUDPPorts = [ 22 ];
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
          routes = [
            {
              address = "::";
              prefixLength = 0;
            }
          ];
          addresses = [
            {
              address = "2001:470:35:606::2";
              prefixLength = 64;
            }
          ];
        };
      };
    };
    wg-quick = {
      interfaces =
        let
          privateKeyFile = config.sops.secrets."wireguard/tcloud".path;
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
        in
        {
          wg_office = {
            inherit address privateKeyFile table;
            listenPort = 22616;
            postUp = ''
              ${pkgs.iproute2}/bin/ip addr add dev wg_office 172.22.240.97/32 peer 172.22.240.98/32
              ${pkgs.iproute2}/bin/ip addr add dev wg_office fd48:4b4:f3::1/128 peer fd48:4b4:f3::2/128
              ${pkgs.iproute2}/bin/ip link set multicast on dev wg_office
            '';
            peers = [
              {
                publicKey = shares.hosts-dict.office.wg.public-key;
                inherit allowedIPs;
              }
            ];
          };
          wg_game = {
            inherit address privateKeyFile table;
            listenPort = 22617;
            postUp = ''
              ${pkgs.iproute2}/bin/ip addr add dev wg_game 172.22.240.97/32 peer 172.22.240.99/32
              ${pkgs.iproute2}/bin/ip addr add dev wg_game fd48:4b4:f3::1/128 peer fd48:4b4:f3::3/128
              ${pkgs.iproute2}/bin/ip link set multicast on dev wg_game
            '';

            peers = [
              {
                publicKey = shares.hosts-dict.game.wg.public-key;
                inherit allowedIPs;
              }
            ];
          };
          wg_game_office = {
            inherit address privateKeyFile table;
            listenPort = 22618;
            postUp = ''
              ${pkgs.iproute2}/bin/ip addr add dev wg_game_office 172.22.240.97/32 peer 172.22.240.100/32
              ${pkgs.iproute2}/bin/ip addr add dev wg_game_office fd48:4b4:f3::1/128 peer fd48:4b4:f3::4/128
              ${pkgs.iproute2}/bin/ip link set multicast on dev wg_game_office
            '';
            peers = [
              {
                publicKey = "nBEkTpn4kRYXS9r7beXh3uMYJBAq/534byXv8NsB8gM=";
                inherit allowedIPs;
              }
            ];
          };
          wg_iphone = {
            inherit address privateKeyFile table;
            listenPort = 22619;
            postUp = ''
              ${pkgs.iproute2}/bin/ip addr add dev wg_iphone 172.22.240.97/32 peer 172.22.240.101/32
              ${pkgs.iproute2}/bin/ip addr add dev wg_iphone fd48:4b4:f3::1/128 peer fd48:4b4:f3::5/128
              ${pkgs.iproute2}/bin/ip link set multicast on dev wg_iphone
            '';
            peers = [
              {
                publicKey = "CAW6+atqM9xmCAZUaev3OZWbYKwjDNCHezyiBpiHmSg=";
                inherit allowedIPs;
              }
            ];
          };

          wg_sg_office = {
            inherit address privateKeyFile table;
            listenPort = 22620;
            postUp = ''
              ${pkgs.iproute2}/bin/ip addr add dev wg_sg_office 172.22.240.97/32 peer 172.22.240.102/32
              ${pkgs.iproute2}/bin/ip addr add dev wg_sg_office fd48:4b4:f3::1/128 peer fd48:4b4:f3::6/128
              ${pkgs.iproute2}/bin/ip link set multicast on dev wg_sg_office 
            '';
            peers = [
              {
                publicKey = "9WkAJx+EG3VifVLiMgD8+6CoCsBwSyWAMwtajoy/OTk=";
                inherit allowedIPs;
              }
            ];
          };
          wg_kioubit = {
            inherit privateKeyFile address table;
            peers = [
              {
                endpoint = "hk1.g-load.eu:22616";
                publicKey = "sLbzTRr2gfLFb24NPzDOpy8j09Y6zI+a7NkeVMdVSR8=";
                inherit allowedIPs;
              }
            ];
          };
        };
    };
  };

  virtualisation = {
    oci-containers = {
      containers =
        let
          image = "vinlic/deepseek-free-api:latest";
          environment = {
            TZ = "Asia/Shanghai";
          };
        in
        {
          deepseek-free-api = {
            inherit environment image;
            ports = [ "8000:8000" ];
          };
        };
    };
  };

  services = {

    owncast = {
      enable = false; # Disabled: owncast 0.2.3 crash-loops due to Go runtime panic (minpc/maxpc invalid)
      openFirewall = true;
      listen = "0.0.0.0";
    };

    postgresql = {
      enable = true;
      ensureUsers = [
        {
          name = "freeman.xiong";
          ensureDBOwnership = true;
          ensureClauses = {
            superuser = true;
          };
        }
      ];
      ensureDatabases = [ "freeman.xiong" ];
    };

    coturn = {
      enable = true;
      realm = "tcloud.${config.networking.domain}";
      extraConfig = ''
        fingerprint
        no-software-attribute
      '';
      lt-cred-mech = true;
      no-cli = true;
    };

    journald = {
      extraConfig = ''
        Storage=volatile
        RuntimeMaxUse=30M
      '';
    };

    openssh =
      let
        file = pkgs.writeText "ca.pub" "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBEpEaKdRToEGqji1PLnZsP+AaScTYQbcdkLCYYPe+gYX5ILxcuHXL+O5GdHzs+LtC6csdvzzBQBaJEpT7pr/GsM=";
      in
      {
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
  };
}
