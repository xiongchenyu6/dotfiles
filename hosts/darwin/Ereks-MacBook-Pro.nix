# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  profiles,
  suites,
  ...
}: {
  imports =
    [
      profiles.users.chjiang
      profiles.users.root.darwin
    ]
    ++ suites.base;
  activate-system.enable = true;
  homebrew = {
    enable = true;
    global = {
      autoUpdate = true;
      brewfile = true;
    };
    taps = ["homebrew/services" "homebrew/core" "homebrew/cask" "homebrew/cask-drivers"];
    onActivation = {
      autoUpdate = true;
      cleanup = "none";
      upgrade = true;
    };
  };
}
