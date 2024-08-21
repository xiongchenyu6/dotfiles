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
  ];

  programs = {
    zsh = {
      enable = true;
    };
    tmux = {
      enable = true;
    };
  };

  fonts = {
    fontDir = {
      enable = true;
    };
    packages = with pkgs; [
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-cjk-serif
      sarasa-gothic # 更纱黑体
      wqy_microhei
      wqy_zenhei
      (nerdfonts.override { fonts = [ "Hack" ]; })
      jetbrains-mono
      emacs-all-the-icons-fonts
      font-awesome
    ];
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
        "auto-allocate-uids"
        "cgroups"
        "parse-toml-timestamps"
        "recursive-nix"
      ];
      trusted-users = [ "freeman.xiong" ];
      allowed-users = [
        "root"
        "freeman.xiong"
      ];
      auto-allocate-uids = true;
      auto-optimise-store = true;
      substituters = [
        "https://cache.nixos.org"
        "https://nix-community.cachix.org"
        "https://xddxdd.cachix.org"
        "https://xiongchenyu6.cachix.org"
        "https://hyprland.cachix.org"
        "https://cache.garnix.io"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "xddxdd.cachix.org-1:ay1HJyNDYmlSwj5NXQG065C8LfoqqKaTNCyzeixGjf8="
        "xiongchenyu6.cachix.org-1:mpOGlINmMwc2gb3xb1BjVmhzR8BYWzWYlg4xlTiBr7Q="
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
        "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
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
