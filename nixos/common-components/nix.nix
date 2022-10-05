{ config, pkgs, options, lib, ... }:

{
  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = true;
    };
  };

  nix = {

    daemonCPUSchedPolicy = "idle";
    daemonIOSchedClass = "idle";
    gc = {
      automatic = true;
      options = "--delete-older-than 7d";
      randomizedDelaySec = "1h";
    };
    optimise.automatic = true;
    settings = {
      experimental-features = [
        "nix-command"
        "flakes"
        "repl-flake"
        "ca-derivations"
      ];
      trusted-users = [
        "root"
        "freeman"
      ];

      auto-optimise-store = true;
      substituters = [
        "https://nix-community.cachix.org"
        "https://xddxdd.cachix.org"
        "https://colmena.cachix.org"
        "https://cache.nixos.org/"
        "https://xiongchenyu6.cachix.org"
      ];
      trusted-public-keys = [
        "xddxdd.cachix.org-1:ay1HJyNDYmlSwj5NXQG065C8LfoqqKaTNCyzeixGjf8="
        "colmena.cachix.org-1:7BzpDnjjH8ki2CT3f6GdOk7QAzPOl+1t3LvTLXqYcSg="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "xiongchenyu6.cachix.org-1:mpOGlINmMwc2gb3xb1BjVmhzR8BYWzWYlg4xlTiBr7Q="
      ];
    };
  };
}

