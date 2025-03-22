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
        "text/html" = "microsoft-edge.desktop";
        "text/x-csharp" = [ "rider.desktop" ];
        "x-scheme-handler/http" = "microsoft-edge.desktop";
        "x-scheme-handler/https" = "microsoft-edge.desktop";
        "x-scheme-handler/about" = "microsoft-edge.desktop";
        "x-scheme-handler/unknown" = "microsoft-edge.desktop";
      };
    };
  };
}
