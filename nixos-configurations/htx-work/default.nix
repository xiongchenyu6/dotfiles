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
    ezModules.seanhxx
    ezModules.misc
    ezModules.client-cli
    ezModules.gui
    ezModules.core
    ezModules.greetd
    # #ezModules.datadog-agent
    ezModules.virtualisation
    nixos-hardware.nixosModules.lenovo-legion-16ach6h
    srvos.nixosModules.desktop
    vscode-server.nixosModules.default
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
  ];

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
    loader = {
      systemd-boot = {
        configurationLimit = 12;
        enable = true;
      };
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot";
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
        enable = false;
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
      };

      networkmanager = {
        enable = true;
        wifi = {
          powersave = true;
        };
      };
      enableIPv6 = true;
      useDHCP = lib.mkDefault true;
    };

  services = {
    greetd = {
      settings = {
        initial_session = {
          user = "seanhxx";
          command = lib.mkDefault "Hyprland";
        };
      };
    };

    postgresql = {
      enable = true;
      ensureUsers = [
        {
          name = "seanhxx";
          ensureDBOwnership = true;
          ensureClauses = {
            superuser = true;
          };
        }
      ];
      ensureDatabases = [ "seanhxx" ];
    };

    netbird = {
      enable = true;
    };
  };

  home-manager = {
    users = {
      "seanhxx" = {
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
