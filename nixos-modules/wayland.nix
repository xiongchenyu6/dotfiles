{ pkgs, ... }:
{
  xdg = {
    portal = {
      config = {
        # common = {
        #   default = "*";
        # };
        hyprland = {
          "org.freedesktop.impl.portal.ScreenCast" = "hyprland";
        };
      };

      enable = true;
      wlr = {
        enable = true;
      };
      lxqt = {
        enable = true;
      };
      xdgOpenUsePortal = true;
      extraPortals = with pkgs; [
        xdg-desktop-portal-hyprland
      ];
    };
  };
}
