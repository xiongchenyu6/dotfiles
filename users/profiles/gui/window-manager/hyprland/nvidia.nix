{ lib, ... }: {
  home = {
    sessionVariables = {
      WLR_NO_HARDWARE_CURSORS = "1";
      "__GLX_VENDOR_LIBRARY_NAME" = "nvidia";
    };
  };

  wayland = {
    windowManager = {
      hyprland = {
        extraConfig = lib.mkDefault ''
          env = LIBVA_DRIVER_NAME,nvidia
          env = XDG_SESSION_TYPE,wayland
          env = GBM_BACKEND,nvidia-drm
        '';
      };
    };
  };
}
