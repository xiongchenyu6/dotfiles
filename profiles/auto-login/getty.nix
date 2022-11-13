{pkgs, ...}: {
  services = {
    getty = {
      autologinUser = "freeman";
    };
  };
  environment = {
    systemPackages = with pkgs; [
      wofi
      grim
      slurp
      brightnessctl
      hyprpaper
      gnomeExtensions.zoom-wayland-extension
      wl-clipboard
      xdg_utils
    ];
  };
  xdg = {
    portal = {
      enable = true;
      wlr = {
        enable = true;
      };
      extraPortals = with pkgs; [
        xdg-desktop-portal-gtk
      ];
    };
  };
}
