{ inputs, pkgs, ... }:
{
  imports = [
    inputs.niri.nixosModules.niri
  ];

  programs.niri = {
    enable = true;
    # Use niri-unstable (main) for `background-effect { xray ... }` and other
    # post-25.08 features. niri-flake's `niri-stable` is pinned to v25.08
    # which doesn't yet support background effects in window/layer rules.
    package = inputs.niri.packages.${pkgs.stdenv.hostPlatform.system}.niri-unstable;
  };

  xdg = {
    portal = {
      enable = true;
      config = {
        common = {
          default = "*";
        };
      };
      wlr = {
        enable = true;
      };
      lxqt = {
        enable = true;
      };
      xdgOpenUsePortal = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-gtk
        xdg-desktop-portal-gnome
      ];
    };
    terminal-exec = {
      enable = true;
    };
    mime = {
      defaultApplications = {
        "text/html" = "google-chrome.desktop";
        "text/x-csharp" = [ "rider.desktop" ];
        "x-scheme-handler/http" = "google-chrome.desktop";
        "x-scheme-handler/https" = "google-chrome.desktop";
        "x-scheme-handler/about" = "google-chrome.desktop";
        "x-scheme-handler/unknown" = "google-chrome.desktop";
        "x-scheme-handler/claude" = "claude-desktop.desktop";
      };
    };
  };
}
