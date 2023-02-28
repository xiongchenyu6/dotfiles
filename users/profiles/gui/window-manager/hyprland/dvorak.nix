{ lib, ... }: {
  wayland = {
    windowManager = {
      hyprland = {
        extraConfig = lib.mkDefault ''
          # device:topre-corporation-hhkb-professional {
          input {
          kb_variant = dvp
          kb_model = dvorak-programmer
          }

          bind= $mainMod, 14,moveworkspacetomonitor,1 current
          bind = $mainMod, 14, workspace, 1
          bind= $mainMod, 17,moveworkspacetomonitor,2 current
          bind = $mainMod, 17, workspace, 2
          bind= $mainMod, 13,moveworkspacetomonitor,3 current
          bind = $mainMod, 13, workspace, 3
          bind= $mainMod, 18,moveworkspacetomonitor,4 current
          bind = $mainMod, 18, workspace, 4
          bind= $mainMod,12,moveworkspacetomonitor,5 current
          bind = $mainMod, 12, workspace, 5
          bind= $mainMod, 19,moveworkspacetomonitor,6 current
          bind = $mainMod, 19, workspace, 6
          bind= $mainMod, 11,moveworkspacetomonitor,7 current
          bind = $mainMod, 11, workspace, 7
          bind= $mainMod,20,moveworkspacetomonitor,8 current
          bind = $mainMod, 20, workspace, 8
          bind= $mainMod,15,moveworkspacetomonitor,9 current
          bind = $mainMod, 15, workspace, 9
          bind= $mainMod, 16,moveworkspacetomonitor,10 current
          bind = $mainMod, 16, workspace, 10

          bind = $mainMod SHIFT, 14, movetoworkspace, 1
          bind = $mainMod SHIFT, 17, movetoworkspace, 2
          bind = $mainMod SHIFT, 13, movetoworkspace, 3
          bind = $mainMod SHIFT, 18, movetoworkspace, 4
          bind = $mainMod SHIFT, 12, movetoworkspace, 5
          bind = $mainMod SHIFT, 19, movetoworkspace, 6
          bind = $mainMod SHIFT, 11, movetoworkspace, 7
          bind = $mainMod SHIFT, 20, movetoworkspace, 8
          bind = $mainMod SHIFT, 15, movetoworkspace, 9
          bind = $mainMod SHIFT, 16, movetoworkspace, 10
          # }
        '';
      };
    };
  };
}
