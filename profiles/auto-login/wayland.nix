{ pkgs, ... }: {
  environment = {
    systemPackages = with pkgs; [
      grim
      slurp
      brightnessctl
      hyprpaper
      hyprpicker
      wl-clipboard
      wf-recorder
      xdg_utils
      wev
      waypipe
    ];
  };

  xdg = {
    portal = {
      config.common.default = "*";
      enable = true;
      wlr = { enable = true; };
      lxqt = { enable = true; };
      xdgOpenUsePortal = false;
      extraPortals = with pkgs; [
        xdg-desktop-portal-gtk
        xdg-desktop-portal-hyprland
      ];
    };
  };

}
