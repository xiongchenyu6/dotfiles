{ lib, ... }:
{
  home = {
    sessionVariables = {
      "__GLX_VENDOR_LIBRARY_NAME" = "nvidia";
      "GBM_BACKEND" = "nvidia-drm";
      #"AQ_DRM_DEVICES" = "/dev/dri/card0:/dev/dri/card1";
    };
  };

  wayland = {
    windowManager = {
      hyprland = {
        extraConfig = lib.mkDefault ''
          general {
          # See https://wiki.hyprland.org/Configuring/Variables/ for more
          # no_cursor_warps = 1
          env = WLR_NO_HARDWARE_CURSORS,1
          env = LIBVA_DRIVER_NAME,nvidia
          env = XDG_SESSION_TYPE,wayland
          env = NIXOS_OZONE_WL,1
          }
        '';
      };
    };
  };
}
