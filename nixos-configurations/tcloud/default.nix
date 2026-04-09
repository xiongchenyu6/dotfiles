{
  modulesPath,
  inputs,
  lib,
  ezModules,
  config,
  pkgs,
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
    ezModules.datadog-agent
    ezModules.sing-box
    srvos.nixosModules.server
    ezModules.mixins-nginx
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
    ./hardware-configuration.nix
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
  };
}
