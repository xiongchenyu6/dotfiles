{ pkgs, ... }:
{
  xdg = {
    portal = {
      config.common.default = "*";
      enable = true;
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
  };
}
