{ lib, pkgs, ... }:
{
  wayland = {
    windowManager = {
      hyprland = {
        enable = true;

        settings = {
          input = {
            kb_variant = "dvp";
            kb_model = "dvorak-programmer";
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

            "$mod, E, exec, ${pkgs.microsoft-edge}/bin/microsoft-edge"

            "$mod, 14, moveworkspacetomonitor, 1 current"
            "$mod, 14, workspace, 1"
            "$mod, 17, moveworkspacetomonitor, 2 current"
            "$mod, 17, workspace, 2"
            "$mod, 13, moveworkspacetomonitor, 3 current"
            "$mod, 13, workspace, 3"
            "$mod, 18, moveworkspacetomonitor, 4 current"
            "$mod, 18, workspace, 4"
            "$mod, 12, moveworkspacetomonitor, 5 current"
            "$mod, 12, workspace, 5"
            "$mod, 19, moveworkspacetomonitor, 6 current"
            "$mod, 19, workspace, 6"
            "$mod, 11, moveworkspacetomonitor, 7 current"
            "$mod, 11, workspace, 7"
            "$mod, 20, moveworkspacetomonitor, 8 current"
            "$mod, 20, workspace, 8"
            "$mod, 15, moveworkspacetomonitor, 9 current"
            "$mod, 15, workspace, 9"
            "$mod, 16, moveworkspacetomonitor, 10 current"
            "$mod, 16, workspace, 10"

            "$mod SHIFT, 14, movetoworkspace, 1"
            "$mod SHIFT, 17, movetoworkspace, 2"
            "$mod SHIFT, 13, movetoworkspace, 3"
            "$mod SHIFT, 18, movetoworkspace, 4"
            "$mod SHIFT, 12, movetoworkspace, 5"
            "$mod SHIFT, 19, movetoworkspace, 6"
            "$mod SHIFT, 11, movetoworkspace, 7"
            "$mod SHIFT, 20, movetoworkspace, 8"
            "$mod SHIFT, 15, movetoworkspace, 9"
            "$mod SHIFT, 16, movetoworkspace, 10"
          ];
        };
      };
    };
  };
}
