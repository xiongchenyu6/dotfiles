# Edit
{ lib, suites, profiles, ... }: {
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
            };
          };
        };
      };
    };
  };
}
