# Shared core config — nix settings, nixpkgs, timezone (Darwin + NixOS)
{
  lib,
  pkgs,
  config,
  ...
}:
{
  nixpkgs.config = {
    allowUnfree = true;
    android_sdk.accept_license = true;
    permittedInsecurePackages = [
      "openssl-1.1.1w"
      "libxml2-2.13.8"
    ];
  };

  sops.secrets.nixAccessTokens = {
    mode = "0440";
    group = if (config.users.groups ? keys) then config.users.groups.keys.name else "wheel";
  };
  environment = {
    systemPackages = lib.optionals pkgs.stdenv.isLinux [
      pkgs.ghostty.terminfo
    ];

    etc = lib.mkIf pkgs.stdenv.isLinux {
      "ppp/options".text = ''
        ipcp-accept-remote
      '';
    };
  };

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
      options = "--delete-older-than 1d";
    };

    extraOptions = ''
      !include ${config.sops.secrets.nixAccessTokens.path}
    '';

    settings = {
      accept-flake-config = true;
      allow-import-from-derivation = true;
      experimental-features = [
        "nix-command"
        "flakes"
        #"ca-derivations"
        "parse-toml-timestamps"
      ];
      trusted-users = [
        "freeman.xiong"
        "freeman"
        "@wheel"
        "@admin"
      ];
      allowed-users = [
        "root"
        "freeman"
        "freeman.xiong"
        "@wheel"
        "@admin"
      ];
      substituters = [
        "https://xddxdd.cachix.org"
        "https://xiongchenyu6.cachix.org"
        "https://hyprland.cachix.org"
        "https://ghostty.cachix.org"
      ];
      trusted-public-keys = [
        "xddxdd.cachix.org-1:ay1HJyNDYmlSwj5NXQG065C8LfoqqKaTNCyzeixGjf8="
        "xiongchenyu6.cachix.org-1:mpOGlINmMwc2gb3xb1BjVmhzR8BYWzWYlg4xlTiBr7Q="
        "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc="
        "ghostty.cachix.org-1:QB389yTa6gTyneehvqG58y0WnHjQOqgnA+wBnpWWxns="
      ];
    };
    distributedBuilds = lib.mkDefault true;
  };

  time = {
    timeZone = "Asia/Singapore";
  };
}
