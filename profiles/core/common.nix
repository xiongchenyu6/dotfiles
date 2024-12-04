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
    imagemagick
    lrzsz
    home-manager
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
    gc = {
      automatic = true;
      options = "--delete-older-than 1d";
    };

    settings = {
      access-tokens = builtins.readFile ../../secrets/access-key.password;
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
}
