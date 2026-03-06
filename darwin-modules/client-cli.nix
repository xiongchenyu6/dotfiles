# macOS system defaults — keyboard, Finder, Dock, trackpad
{ ... }:
{
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
      trackpad = {
        Clicking = true;
      };
    };
    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };
  };
}
