# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ pkgs, ... }: {
  imports = [ ./common.nix ];
  system = {
    defaults = {
      NSGlobalDomain = {
        InitialKeyRepeat = 18;
        KeyRepeat = 6;
        AppleTemperatureUnit = "Celsius";
        AppleShowAllFiles = true;
      };
      dock = {
        autohide = true;
        dashboard-in-overlay = true;
        mru-spaces = false;
      };
      finder = {
        AppleShowAllExtensions = true;
        AppleShowAllFiles = true;
        QuitMenuItem = true;
        ShowPathbar = true;
        ShowStatusBar = true;
      };
      trackpad = { Clicking = true; };
    };
    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };
  };
}
