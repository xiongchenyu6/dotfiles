{ lib, ... }: {
  home = {
    sessionVariables = {
      WLR_NO_HARDWARE_CURSORS = "1";
      # "__GLX_VENDOR_LIBRARY_NAME" = "nvidia";
      # "LIBVA_DRIVER_NAME" = "nvidia";
      # "XDG_SESSION_TYPE" = "wayland";
      # "GBM_BACKEND" = "nvidia-drm";
    };
  };

  wayland = {
    windowManager = {
      hyprland = {
        enableNvidiaPatches = true;

        extraConfig = lib.mkDefault ''
          general {
          # See https://wiki.hyprland.org/Configuring/Variables/ for more
          # no_cursor_warps = 1
          env = WLR_NO_HARDWARE_CURSORS,1
          # env = LIBVA_DRIVER_NAME,nvidia
          # env = XDG_SESSION_TYPE,wayland
          env = NIXOS_OZONE_WL,1
          }
        '';
      };
    };
  };
}

