# Edit
{ lib, profiles, config, pkgs, mylib, ... }: {
  imports = [
    ./hardware-configuration.nix
    ../../../profiles/core/nixos.nix
    ../../../users/root/nixos.nix
    ../../../profiles/dvorak.nix
    ../../../users/freeman.xiong
    ../../../profiles/hardwares/misc.nix
    ../../../profiles/client/cli/nixos.nix
    ../../../profiles/client/gui/nixos.nix
    ../../../profiles/sops.nix
    ../../../profiles/common/components
    ../../../profiles/auto-login/greetd.nix
    ../../../profiles/common/apps/dn42
    ../../../profiles/common/apps/bird-inner.nix
    ../../../profiles/common/apps/kerberos.nix
    ../../../profiles/common/components/datadog-agent.nix
  ];

  # sops.secrets."wireguard/office" = { };

  sops.secrets."wireguard/game" = { };
  # /nix /var /root /nix/persist

  # Enable users/freeman gui
  system.nixos.tags = [ "with-gui-nvidia" ];

  hardware = { enableRedistributableFirmware = true; };

  nixpkgs = {
    config = {
      permittedInsecurePackages =
        [ "openssl-1.1.1w" "electron-19.1.9" "zotero-6.0.27" ];
    };
  };

  boot = {
    binfmt.emulatedSystems = [ "aarch64-linux" ];
    #    kernelParams = [ "psmouse.synaptics_intertouch=0" ];
    kernel = { sysctl = { "net.ipv4.ip_forward" = 1; }; };

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

  networking = let
    file-path = builtins.split "/" (toString ./.);
    hostName = lib.last file-path;
  in {
    inherit hostName;

    # extraHosts = "54.254.210.117  grafana-oncall.trontech.link";
    firewall = {
      enable = true;
      allowedTCPPorts = [ 89 179 ];
      allowedUDPPorts = [ 89 179 5353 6696 33434 ];
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
        #macAddress = "random";
      };
      #ethernet = { macAddress = "random"; };
    };
    enableIPv6 = true;
    wg-quick = {
      interfaces = {
        wg_mail = {
          privateKeyFile = config.sops.secrets."wireguard/game".path;
          table = "off";
          address = [ "fe80::102/64" ];
          dns = [ "172.20.0.53" ];
          postUp = ''
            ${pkgs.iproute2}/bin/ip addr add dev wg_mail 172.22.240.99/32 peer 172.22.240.96/27
            ${pkgs.iproute2}/bin/ip addr add dev wg_mail fd48:4b4:f3::3/128 peer fd48:4b4:f3::1/128
            ${pkgs.iproute2}/bin/ip link set multicast on dev wg_mail
            ${pkgs.systemd}/bin/resolvectl domain wg_mail dn42.
          '';

          peers = [{
            endpoint = "43.156.66.157:22617";
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
              "224.0.0.251/32" # avahi
              "ff02::fb/128" # avahi
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
      # enable = true;
      interfaces = {
        wg_mail = {
          hello-interval = 5;
          split-horizon = "auto";
          type = "wired";
        };
      };
    };
    bird2 = {
      enable = true;
      config = mylib.bird2-inner-config "172.22.240.99" "fd48:4b4:f3::3";
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
        imports = [
          ../../../users/profiles/gui/nixos.nix
          ../../../users/profiles/gui/mpd.nix
          ../../../users/profiles/gui/window-manager/hyprland/dvorak.nix
          ../../../users/profiles/gui/window-manager/hyprland/nvidia.nix
          ../../../users/profiles/gui/window-manager/hyprland/nixos.nix
        ];

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
