{pkgs, ...}: {
  services = {
    getty = {
      autologinUser = "freeman.xiong";
    };
  };
  environment = {
    systemPackages = with pkgs; [
      wofi
      grim
      slurp
      brightnessctl
      hyprpaper
      hyprpicker
      gnomeExtensions.zoom-wayland-extension
      wl-clipboard
      wf-recorder
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
