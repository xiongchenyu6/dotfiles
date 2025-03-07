# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ pkgs, lib, ... }:
{
  nixpkgs.config = {
    allowUnfree = true;
    allowBroken = true;
    android_sdk.accept_license = true;
    permittedInsecurePackages = [
      "dotnet-sdk-6.0.428"
    ];
  };
  environment = {

    etc = {
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

    settings =
      let
        githubAccessToken = builtins.getEnv "Github_Access_Token";
      in
      if githubAccessToken == "" then
        throw "Github_Access_Token environment variable is not set"
      else
        {
          access-tokens = "github.com=${githubAccessToken}";
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
}
