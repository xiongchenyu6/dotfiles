# Edit
{ lib, suites, profiles, config, ... }: {
  imports = [
    ./hardware-configuration.nix
    profiles.server-apps.mysql
    profiles.core.nixos
    profiles.client-pkgs.nixos
    profiles.users.root.nixos
    profiles.dvorak
    profiles.users."freeman.xiong"
    profiles.hardwares.misc
    # profiles.hardwares.nvidia
  ] ++ suites.client-base ++ suites.client-network;

  sops.secrets."wireguard/office" = { };

  sops.secrets."wireguard/game" = { };
  # /nix /var /root /nix/persist

  # Enable users/freeman gui
  system.nixos.tags = [ "with-gui-nvidia" ];

  hardware = { enableRedistributableFirmware = true; };

  nixpkgs = {
    config = {
      permittedInsecurePackages = [ "electron-19.0.7" ];
      allowBroken = true;
    };
  };

  boot = {
    loader = {
      systemd-boot = {
        enable = true;
        netbootxyz.enable = true;
        memtest86.enable = true;
      };
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/efi";
      };
    };
    tmpOnTmpfs = lib.mkDefault true;
  };

  networking = {
    firewall = {
      allowedTCPPorts = [ 179 ];
      allowedUDPPorts = [ 179 33434 ];
      enable = true;
    };

    networkmanager = {
      enable = true;
      # firewallBackend = "nftables";
      wifi = {
        powersave = true;
        macAddress = "random";
      };
      ethernet = { macAddress = "random"; };
      enableFccUnlock = true;
    };
    enableIPv6 = true;
    wg-quick = {
      interfaces = {
        wg_freeman = {
          privateKeyFile = config.sops.secrets."wireguard/game".path;
          table = "off";
          address = [ "172.22.240.99/27" "fe80::102/64" "fd48:4b4:f3::3/48" ];
          peers = [{
            endpoint = "freeman.engineer:22616";
            publicKey = profiles.share.hosts-dict.mail.wg.public-key;
            persistentKeepalive = 30;
            allowedIPs = [
              "10.0.0.0/8"
              "172.20.0.0/14"
              "172.31.0.0/16"
              "fd00::/8"
              "fe80::/10"
              "fd48:4b4:f3::/48"
              "13.212.219.245"
            ];
          }];
        };
        wg_tronlink = {
          privateKeyFile = config.sops.secrets."wireguard/office".path;
          address = [ "172.64.224.3/24" "fe80::103/64" ];
          peers = [{
            endpoint = "vpn.trontech.link:22617";
            publicKey = profiles.share.hosts-dict.tronlink.wg.public-key;
            persistentKeepalive = 5;
            allowedIPs = [
              "172.64.224.1/24"
              "fe80::101/64"
              "172.32.0.0/16"
              "18.218.96.133/32"
              "13.212.2.33"
            ];
          }];
        };
      };
    };

    useDHCP = lib.mkDefault true;
  };

  services = {
    postgresql = {
      enable = true;
      authentication = ''
        local all all trust
      '';
    };
    bird2 = {
      config = lib.mine.bird2-inner-config "172.22.240.99" "fd48:4b4:f3::3";
    };
  };

  krb5 = {
    realms = let
      tronRealm = "TRONTECH.LINK";
      tronDomain = "trontech.link";
    in {
      "${tronRealm}" = {
        admin_server = "admin.inner.${tronDomain}";
        kdc = [ "admin.inner.${tronDomain}" ];
        default_domain = "admin.inner.${tronDomain}";
        kpasswd_server = "admin.inner.${tronDomain}";
        database_module = "openldap_ldapconf";
      };
      domain_realm = {
        "${tronDomain}" = tronRealm;
        ".inner.${tronDomain}" = tronRealm;
        ".${tronDomain}" = tronRealm;
      };
    };
  };

  home-manager = {
    users = {
      "freeman.xiong" = {
        sops = {
          gnupg = { home = "~/.gnupg"; };
          secrets = {
            # The path to the file to decrypt.
            gptcommit = {
              name = "gptcommit";
              path = "/home/freeman.xiong/.config/gptcommit/config.toml";
              mode = "777";
            };
          };
        };
        services = {
          git-sync = {
            enable = true;
            repositories = {
              notes = {
                path = "/home/freeman.xiong/Private/xiongchenyu6.github.io";
                uri = "git@github.com:xiongchenyu6/xiongchenyu6.github.io.git";
                # interval = 10;
              };
            };
          };
        };
        programs = {
          waybar = { settings = { network = { interface = "wlp4s0"; }; }; };
        };
      };
    };
  };
}
