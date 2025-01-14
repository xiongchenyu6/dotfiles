# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ pkgs, lib, ... }:
{
  
  imports = [ ./common.nix ];

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

  nix = {
    daemonCPUSchedPolicy = "idle";
    daemonIOSchedClass = "idle";
    gc = {
      randomizedDelaySec = "1h";
    };
    optimise = {
      automatic = true;
    };
  };

  system = {
    stateVersion = "25.05";
  }; # Did you read the comment?

}
