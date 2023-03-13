{ pkgs, ... }: {
  imports = [ ./wayland.nix ];

  services = {
    greetd = {
      enable = true;
      settings = rec {
        initial_session = let
          s = pkgs.writeShellApplication {
            name = "s.sh";
            runtimeInputs = [ pkgs.hyprland ];
            text = ''
              export WLR_NO_HARDWARE_CURSORS=1;
              export LIBVA_DRIVER_NAME=nvidia
              export XDG_SESSION_TYPE=wayland
              export GBM_BACKEND=nvidia-drm
              export __GLX_VENDOR_LIBRARY_NAME=nvidia
              Hyprland
            '';
          };
        in {
          user = "freeman.xiong";
          command = "${s}/bin/s.sh";
        };
        default_session = initial_session;
      };
    };
  };
}
