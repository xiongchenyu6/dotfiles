# Edit
{
  inputs,
  lib,
  ezModules,
  config,
  pkgs,
  mylib,
  shares,
  ...
}:
{
  imports = with inputs; [
    ./hardware-configuration.nix
    ezModules.root
    ezModules.dvorak
    ezModules."freeman.xiong"
    ezModules.misc
    ezModules.client-cli
    ezModules.gui
    ezModules.vr
    ezModules.core
    ezModules.greetd
    ezModules.dn42
    ezModules.bird-inner
    # #ezModules.datadog-agent
    ezModules.virtualisation
    nixos-hardware.nixosModules.lenovo-legion-16ach6h
    srvos.nixosModules.desktop
    vscode-server.nixosModules.default
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
    {
      topology.self.interfaces.home = {
        type = "wireguard";
        addresses = [ "172.22.240.100/27" ];
      };
    }

  ];

  # sops.secrets."wireguard/office" = { };
  sops.secrets."wireguard/game-office" = { };
  # /nix /var /root /nix/persist

  # Enable users/freeman gui
  system.nixos.tags = [
    "nvidia"
    "gui"
  ];

  hardware = {
    enableRedistributableFirmware = true;
  };

  boot = {
    kernelPackages = pkgs.linuxPackages_6_12;
    binfmt.emulatedSystems = [ "aarch64-linux" ];
    kernel = {
      sysctl = {
        "net.ipv4.ip_forward" = 1;
      };
    };

    loader = {
      systemd-boot = {
        configurationLimit = 12;
        enable = true;
        # netbootxyz.enable = true;
        # memtest86.enable = true;
      };
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot";
      };
      # grub = {
      #   enable = true;
      #   efiSupport = true;
      #   device = "nodev";
      #   configurationLimit = 5;
      #   useOSProber = true;
      # };
    };
  };

  networking =
    let
      file-path = builtins.split "/" (toString ./.);
      hostName = lib.last file-path;
    in
    {
      inherit hostName;

      # extraHosts = "54.254.210.117  grafana-oncall.trontech.link";
      firewall = {
        enable = true;
        allowedTCPPorts = [
          89
          179
          5002
        ];
        allowedUDPPorts = [
          89
          179
          5353
          6696
          33434
        ];
        interfaces.wg_mail.allowedTCPPorts = [
          22
          8080
        ];
        interfaces.wg_mail.allowedUDPPorts = [ 22 ];
        interfaces.wt0.allowedTCPPorts = [ 22 ];
        interfaces.wt0.allowedUDPPorts = [ 22 ];
      };

      networkmanager = {
        enable = true;
        # firewallBackend = "nftables";
        wifi = {
          powersave = true;
          #macAddress = "random";
        };
        #ethernet = { macAddress = "random"; };
      };
      enableIPv6 = true;
      wg-quick = {
        interfaces = {
          wg_mail = {
            privateKeyFile = config.sops.secrets."wireguard/game-office".path;
            table = "off";
            address = [ "fe80::102/64" ];
            #dns = [ "172.20.0.53" ];
            postUp = ''
              ${pkgs.iproute2}/bin/ip addr add dev wg_mail 172.22.240.100/32 peer 172.22.240.96/27
              ${pkgs.iproute2}/bin/ip addr add dev wg_mail fd48:4b4:f3::4/128 peer fd48:4b4:f3::1/128
              ${pkgs.iproute2}/bin/ip link set multicast on dev wg_mail
            '';

            peers = [
              {
                endpoint = "43.156.66.157:22618";
                publicKey = shares.hosts-dict.mail.wg.public-key;
                persistentKeepalive = 30;
                allowedIPs = [
                  "10.0.0.0/8"
                  "172.20.0.0/14"
                  "172.31.0.0/16"
                  "fd00::/8"
                  "fe80::/10"
                  "fd48:4b4:f3::/48"
                  "ff02::1:6/128"
                  "224.0.0.251/32" # avahi
                  "ff02::fb/128" # avahi
                ];
              }
            ];
          };
        };
      };

      useDHCP = lib.mkDefault true;
    };

  services = {

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

    netbird = {
      enable = true;
    };

    bird2 = {
      enable = true;
      config = mylib.bird2-inner-config "172.22.240.100" "fd48:4b4:f3::4";
    };
  };

  home-manager = {
    users = {
      "freeman.xiong" = {
        programs = {
          waybar = {
            settings = {
              network = {
                interface = "wlp4s0";
              };
            };
          };
        };
      };
    };
  };
}
