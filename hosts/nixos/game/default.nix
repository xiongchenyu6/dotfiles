# Edit
{ lib, suites, profiles, config, pkgs, ... }: {
  imports = [
    ./hardware-configuration.nix
    profiles.server-apps.mysql
    profiles.core.nixos
    profiles.client-pkgs.nixos
    profiles.users.root.nixos
    profiles.dvorak
    profiles.users."freeman.xiong"
    profiles.hardwares.misc
    profiles.hardwares.nvidia
  ] ++ suites.client-base ++ suites.client-network;

  sops.secrets."wireguard/office" = { };

  sops.secrets."wireguard/game" = { };
  # /nix /var /root /nix/persist

  # Enable users/freeman gui
  system.nixos.tags = [ "with-gui-nvidia" ];

  hardware = { enableRedistributableFirmware = true; };

  nixpkgs = {
    config = {
      permittedInsecurePackages = [ "openssl-1.1.1u" "electron-19.0.7" ];
      allowBroken = true;
    };
  };

  boot = {
    binfmt.emulatedSystems = [ "aarch64-linux" ];

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

    loader = {
      systemd-boot = {
        configurationLimit = 12;
        enable = true;
        # netbootxyz.enable = true;
        # memtest86.enable = true;
      };
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/efi";
      };
    };
    tmp.useTmpfs = lib.mkDefault true;
  };

  networking = {
    # extraHosts = "13.59.171.215       winklink.org";
    nameservers = [ "8.8.8.8" "10.23.0.10" ];
    firewall = {
      allowedTCPPorts = [ 89 179 ];
      allowedUDPPorts = [ 89 179 6696 33434 ];
      enable = true;
      interfaces.wg_mail.allowedTCPPorts = [ 2222 ];
      interfaces.wg_mail.allowedUDPPorts = [ 2222 ];

      interfaces.wt0.allowedTCPPorts = [ 2222 ];
      interfaces.wt0.allowedUDPPorts = [ 2222 ];
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
        wg_mail = {
          privateKeyFile = config.sops.secrets."wireguard/game".path;
          table = "off";
          address = [ "fe80::102/64" ];
          postUp = ''
            ${pkgs.iproute2}/bin/ip addr add dev wg_mail 172.22.240.99 peer 172.22.240.97
            # ${pkgs.iproute2}/bin/ip addr add dev wg_mail fe80::101 peer fd48:4b4:f3::1
          '';

          peers = [{
            endpoint = "mail.freeman.engineer:22617";
            publicKey = profiles.share.hosts-dict.mail.wg.public-key;
            persistentKeepalive = 30;
            allowedIPs = [
              "10.0.0.0/8"
              "172.20.0.0/14"
              "172.31.0.0/16"
              "fd00::/8"
              "fe80::/10"
              "fd48:4b4:f3::/48"
              "ff02::1:6/128"
            ];
          }];
        };
        # wg_tronlink = {
        #   privateKeyFile = config.sops.secrets."wireguard/office".path;
        #   address = [ "172.64.224.3/24" "fe80::103/64" ];
        #   peers = [{
        #     endpoint = "vpn.trontech.link:22617";
        #     publicKey = profiles.share.hosts-dict.tronlink.wg.public-key;
        #     persistentKeepalive = 5;
        #     allowedIPs = [
        #       "172.64.224.1/24"
        #       "fe80::101/64"
        #       "172.32.0.0/16"
        #       "18.218.96.133/32"
        #       "13.212.2.33"
        #     ];
        #   }];
        # };
      };
    };

    useDHCP = lib.mkDefault true;
  };

  services = {
    netbird = { enable = true; };
    babeld = {
      enable = true;
      interfaces = {
        wg_mail = {
          hello-interval = 5;
          split-horizon = "auto";
          type = "wired";
        };
      };
    };

    postgresql = {
      package = pkgs.postgresql_15;

      enable = true;
      authentication = ''
        local all all trust
      '';
    };
    bird2 = {
      enable = false;
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
