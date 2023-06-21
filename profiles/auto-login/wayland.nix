{ pkgs, ... }: {
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
      wev
      waypipe
    ];
  };
  xdg = {
    portal = {
      enable = true;
      # wlr = { enable = true; };
      extraPortals = with pkgs; [
        xdg-desktop-portal-gtk
        xdg-desktop-portal-hyprland
      ];
    };
  };

}
