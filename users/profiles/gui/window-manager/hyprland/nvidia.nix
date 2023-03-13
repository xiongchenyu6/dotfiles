_: {
  home = {
    sessionVariables = {
      WLR_NO_HARDWARE_CURSORS = "1";
      "__GLX_VENDOR_LIBRARY_NAME" = "nvidia";
      "LIBVA_DRIVER_NAME" = "nvidia";
      "XDG_SESSION_TYPE" = "wayland";
      "GBM_BACKEND" = "nvidia-drm";
    };
  };
  wayland = { windowManager = { hyprland = { nvidiaPatches = true; }; }; };
}

