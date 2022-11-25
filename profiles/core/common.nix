# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{pkgs, ...}: {
  environment.systemPackages = with pkgs; [
    tree
    litecli
    ssh-to-age
    imagemagick
  ];

  programs = {
    zsh = {enable = true;};
    tmux = {enable = true;};
  };

  fonts = {
    fontDir = {enable = true;};
    fonts = with pkgs; [
      wqy_microhei
      wqy_zenhei
      (nerdfonts.override {fonts = ["Hack"];})
      jetbrains-mono
      emacs-all-the-icons-fonts
      font-awesome
    ];
  };

  nix = {
    gc = {
      automatic = true;
      options = "--delete-older-than 7d";
    };
    settings = {
      allow-import-from-derivation = true;
      experimental-features = ["nix-command" "flakes" "repl-flake" "ca-derivations"];
      trusted-users = ["root" "freeman"];

      auto-optimise-store = true;
      substituters = [
        "https://cache.nixos.org"
        "https://nix-community.cachix.org"
        "https://xddxdd.cachix.org"
        "https://colmena.cachix.org"
        "https://xiongchenyu6.cachix.org"
        "https://hyprland.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "xddxdd.cachix.org-1:ay1HJyNDYmlSwj5NXQG065C8LfoqqKaTNCyzeixGjf8="
        "colmena.cachix.org-1:7BzpDnjjH8ki2CT3f6GdOk7QAzPOl+1t3LvTLXqYcSg="
        "xiongchenyu6.cachix.org-1:mpOGlINmMwc2gb3xb1BjVmhzR8BYWzWYlg4xlTiBr7Q="
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
      ];
    };
    distributedBuilds = false;
    buildMachines = [
      {
        hostName = "hydra.inner.trontech.link";
        # sshUser = "michael.yang";
        systems = ["x86_64-linux"];
        maxJobs = 2;
      }
    ];
  };

  time = {timeZone = "Asia/Singapore";};
}
