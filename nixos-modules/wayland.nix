{ pkgs, ... }:
{
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
        xdg-desktop-portal-hyprland
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
