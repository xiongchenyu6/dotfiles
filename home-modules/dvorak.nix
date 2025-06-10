{ lib, pkgs, ... }:
{
  wayland = {
    windowManager = {
      hyprland = {
        enable = true;

        settings = {
          input = {
            kb_variant = "dvorak";
            kb_model = "";
          };

          "$mod" = "SUPER";

          bind = [
            "$mod SHIFT, A, exec, ${
              pkgs.writeShellApplication {
                name = "screen-shot-and-save";
                text = ''
                  TIME=$(date +%Y%m%d-%H%M%S)_screenshot
                  grim -g "$(slurp)" ~/Pictures/"$TIME".png
                '';
              }
            }/bin/screen-shot-and-save"

            "$mod SHIFT, S, exec, ${
              pkgs.writeShellApplication {
                name = "screen-shot";
                text = ''
                  grim -g "$(slurp)" - | wl-copy -o
                '';
              }
            }/bin/screen-shot"

            "$mod, E, exec, ${pkgs.google-chrome}/bin/google-chrome-stable --ozone-platform=wayland  --enable-wayland-ime"

            "$mod, 1, moveworkspacetomonitor, 1 current"
            "$mod, 1, workspace, 1"
            "$mod, 2, moveworkspacetomonitor, 2 current"
            "$mod, 2, workspace, 2"
            "$mod, 3, moveworkspacetomonitor, 3 current"
            "$mod, 3, workspace, 3"
            "$mod, 4, moveworkspacetomonitor, 4 current"
            "$mod, 4, workspace, 4"
            "$mod, 5, moveworkspacetomonitor, 5 current"
            "$mod, 5, workspace, 5"
            "$mod, 6, moveworkspacetomonitor, 6 current"
            "$mod, 6, workspace, 6"
            "$mod, 7, moveworkspacetomonitor, 7 current"
            "$mod, 7, workspace, 7"
            "$mod, 8, moveworkspacetomonitor, 8 current"
            "$mod, 8, workspace, 8"
            "$mod, 9, moveworkspacetomonitor, 9 current"
            "$mod, 9, workspace, 9"
            "$mod, 0, moveworkspacetomonitor, 10 current"
            "$mod, 0, workspace, 10"

            "$mod SHIFT, 1, movetoworkspace, 1"
            "$mod SHIFT, 2, movetoworkspace, 2"
            "$mod SHIFT, 3, movetoworkspace, 3"
            "$mod SHIFT, 4, movetoworkspace, 4"
            "$mod SHIFT, 5, movetoworkspace, 5"
            "$mod SHIFT, 6, movetoworkspace, 6"
            "$mod SHIFT, 7, movetoworkspace, 7"
            "$mod SHIFT, 8, movetoworkspace, 8"
            "$mod SHIFT, 9, movetoworkspace, 9"
            "$mod SHIFT, 0, movetoworkspace, 10"
          ];
        };
      };
    };
  };
}
