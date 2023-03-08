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
  ] ++ suites.client-base;

  # /nix /var /root /nix/persist

  # Enable users/freeman gui
  system.nixos.tags = [ "with-gui" ];

  hardware = { enableRedistributableFirmware = true; };

  nixpkgs = {
    config = {
      permittedInsecurePackages = [ "electron-19.0.7" ];
      allowBroken = true;
    };
  };

  boot = {
    loader = {
      systemd-boot = { editor = false; };
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/efi";
      };
      grub = {
        enable = true;
        efiSupport = true;
        version = 2;
        device = "nodev";
        configurationLimit = 5;
        useOSProber = true;
      };
      grub2-theme = {
        enable = true;
        icon = "white";
        theme = "whitesur";
        screen = "1080p";
        splashImage = ./grub.jpg;
        footer = true;
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
    #hostName = "office"; # Define your hostname.
    # Enable networking
    #networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

    useDHCP = lib.mkDefault true;
  };
  users = {
    mutableUsers = true;
    users = { "freeman.xiong" = { passwordFile = lib.mkForce null; }; };
  };

  home-manager = { users = { "freeman.xiong" = { }; }; };
}
