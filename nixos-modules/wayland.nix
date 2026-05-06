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
      # The shipped *.portal files have UseIn= whitelists that don't include
      # "niri", so without explicit routing here, no backend implements
      # ScreenCast under XDG_CURRENT_DESKTOP=niri and Chrome/Firefox show no
      # window/screen options. niri-specific config overrides UseIn=.
      config = {
        common = {
          default = [ "gtk" ];
        };
        niri = {
          default = [ "gnome" "gtk" ];
          # xdg-desktop-portal-gnome 46+ delegates FileChooser/AppChooser to
          # Nautilus (org.gnome.NautilusPortal). Without Nautilus installed,
          # the D-Bus name isn't activatable and every file picker fails with
          # "Delegated FileChooser call failed: The name is not activatable",
          # breaking Slack/Chromium uploads. Pin these to gtk.
          "org.freedesktop.impl.portal.FileChooser" = [ "gtk" ];
          "org.freedesktop.impl.portal.AppChooser" = [ "gtk" ];
          "org.freedesktop.impl.portal.ScreenCast" = [ "wlr" ];
          "org.freedesktop.impl.portal.Screenshot" = [ "wlr" ];
          "org.freedesktop.impl.portal.Secret" = [ "gnome-keyring" ];
        };
      };
      # Niri implements wlr-screencopy-v1 but is not wlroots-based, and
      # xdg-desktop-portal-gnome's ScreenCast needs Mutter. xdg-desktop-portal-wlr
      # is what actually serves Chrome/Firefox window-share requests under niri.
      wlr = {
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
