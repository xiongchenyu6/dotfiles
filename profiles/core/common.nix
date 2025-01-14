# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ pkgs, lib, ... }:
{

  imports = [ ../tty.nix ];

  environment.systemPackages = with pkgs; [
    tree
    litecli
    ssh-to-age
    lrzsz
    home-manager
    dmidecode
  ];

  programs = {
    zsh = {
      enable = true;
    };
  };

  nix = {
    # sshServe = {
    #   enable = true;
    #   write = true;
    # };
    optimise = {
      automatic = true;
    };

    gc = {
      automatic = true;
      randomizedDelaySec = "1h";
      options = "--delete-older-than 1d";
    };

    settings = {
      access-tokens = "github.com=${builtins.getEnv "Github_Access_Token"}";
      accept-flake-config = true;
      allow-import-from-derivation = true;
      experimental-features = [
        "nix-command"
        "flakes"
        "ca-derivations"
        "parse-toml-timestamps"
      ];
      trusted-users = [
        "freeman.xiong"
      ];
      allowed-users = [
        "root"
        "freeman.xiong"
      ];
      auto-optimise-store = true;
      substituters = [
        # "https://cache.nixos.org"
        "https://xddxdd.cachix.org"
        "https://xiongchenyu6.cachix.org"
        "https://hyprland.cachix.org"
      ];
      trusted-public-keys = [
        "xddxdd.cachix.org-1:ay1HJyNDYmlSwj5NXQG065C8LfoqqKaTNCyzeixGjf8="
        "xiongchenyu6.cachix.org-1:mpOGlINmMwc2gb3xb1BjVmhzR8BYWzWYlg4xlTiBr7Q="
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
      ];
    };
    distributedBuilds = lib.mkDefault true;
  };

  time = {
    timeZone = "Asia/Singapore";
  };
  environment = {
    etc = {
      "ppp/options".text = ''
        ipcp-accept-remote
      '';
    };
  };

  networking = {
    domain = "auto-life.tech";
  };

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
    supportedLocales = [
      "zh_CN.UTF-8/UTF-8"
      "en_US.UTF-8/UTF-8"
    ];
  };

}
