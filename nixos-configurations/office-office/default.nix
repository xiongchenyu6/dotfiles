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
    ezModules.core
    ezModules.greetd
    ezModules.dn42
    ezModules.bird-inner
    ezModules.falcon-sensor
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
    {
      topology.self.interfaces.home = {
        type = "wireguard";
        addresses = [ "172.22.240.98/27" ];
      };
    }
  ];

  # Enable users/freeman gui
  system.nixos.tags = [
    "gui"
  ];

  sops.secrets."wireguard/office" = { };

  hardware = {
    enableRedistributableFirmware = true;
  };

  nix = {
    distributedBuilds = false;
    buildMachines = [
      {
        hostName = "hydra.inner.trontech.link";
        sshUser = "freeman.xiong";
        systems = [ "x86_64-linux" ];
        sshKey = "/home/freeman.xiong/.ssh/id_ed25519";
        maxJobs = 2;
      }
    ];
  };

  nixpkgs = {
    config = {
      permittedInsecurePackages = [
        "openssl-1.1.1w"
        "electron-19.1.9"
        "zotero-6.0.27"
      ];
      allowBroken = true;
    };
  };

  boot = {
    kernelPackages = lib.mkForce pkgs.linuxPackages_latest;
    binfmt.emulatedSystems = [ "aarch64-linux" ];
    supportedFilesystems = [ "nfs4" ];

    kernel = {
      sysctl = {
        "net.ipv4.ip_forward" = 1;
      };
    };

    extraModprobeConfig = ''
      options snd-intel-dspcfg dsp_driver=1
    '';

    loader = {
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot";
      };
      grub = {
        enable = true;
        efiSupport = true;
        device = "nodev";
        configurationLimit = 5;
        useOSProber = true;
      };
    };
  };

  networking =
    let
      file-path = builtins.split "/" (toString ./.);
      hostName = lib.last file-path;
    in
    {
      inherit hostName;

      firewall = {
        enable = true;
        allowedTCPPorts = [
          89
          179
          8080
        ];
        allowedUDPPorts = [
          89
          179
          5353
          6696
          33434
        ];
        interfaces.wg_mail.allowedTCPPorts = [ 2222 ];
        interfaces.wg_mail.allowedUDPPorts = [ 2222 ];
      };

      networkmanager = {
        enable = true;
        dns = "systemd-resolved";
        wifi = {
          powersave = true;
        };
        ethernet = {
          macAddress = "random";
        };
      };
      enableIPv6 = true;

      wg-quick =
        let
          privateKeyFile = config.sops.secrets."wireguard/office".path;
          table = "off";
        in
        {
          interfaces = {
            wg_mail = {
              inherit privateKeyFile table;
              address = [ "fe80::101/64" ];
              postUp = ''
                ${pkgs.iproute2}/bin/ip addr add dev wg_mail 172.22.240.98/32 peer 172.22.240.96/27
                ${pkgs.iproute2}/bin/ip addr add dev wg_mail fd48:4b4:f3::2/128 peer fd48:4b4:f3::1/128
                ${pkgs.iproute2}/bin/ip link set multicast on dev wg_mail
              '';

              peers = [
                {
                  endpoint = "43.156.66.157:22616";
                  publicKey = shares.hosts-dict.mail.wg.public-key;
                  persistentKeepalive = 30;
                  allowedIPs = [
                    "10.0.0.0/8"
                    "172.20.0.0/14"
                    "172.31.0.0/16"
                    "fd00::/8"
                    "fe80::/10"
                    "fd48:4b4:f3::/48"
                    "ff02::1:6/128" # babeld
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

    netbird.enable = true;

    bird = {
      enable = true;
      config = mylib.bird2-inner-config "172.22.240.98" "fd48:4b4:f3::2";
    };

    geth = {
      test-beacon = {
        enable = false;
        syncmode = "light";
        network = "goerli";
        http = {
          enable = true;
          apis = [
            "eth"
            "net"
            "web3"
            "debug"
          ];
        };
      };
    };
  };

  home-manager = {
    users = {
      "freeman.xiong" = {
        programs = {
          waybar = {
            settings = {
              network = {
                interface = "wlp0s20f3";
              };
            };
          };
        };
      };
    };
  };
}
