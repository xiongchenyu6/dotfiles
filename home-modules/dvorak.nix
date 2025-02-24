{ lib, pkgs, ... }:
{
  wayland = {
    windowManager = {
      hyprland =
        let
          screen-shot = pkgs.writeShellApplication {
            name = "screen-shot.sh";
            text = ''
              grim -g "$(slurp)" - | wl-copy -o
            '';
          };
          # screen shot and save to ~/Pictures
          screen-shot-and-save = pkgs.writeShellApplication {
            name = "screen-shot-and-save.sh";
            # date file named in YYYYMMDD-HHmmss format
            text = ''
              TIME=$(date +%Y%m%d-%H%M%S)_screenshot
              grim -g "$(slurp)" ~/Pictures/"$TIME".png
            '';
          };
        in
        {
          extraConfig = lib.mkDefault ''
            # device:topre-corporation-hhkb-professional {
            input {
            kb_variant = dvp
            kb_model = dvorak-programmer
            }

            bind = $mainMod SHIFT, A, exec, ${screen-shot-and-save}/bin/screen-shot-and-save.sh
            bind = $mainMod SHIFT, S, exec, ${screen-shot}/bin/screen-shot.sh
            bind = $mainMod, E, exec, microsoft-edge --ozone-platform=wayland  --enable-wayland-ime --enable-features=Vulkan
            # bind = $mainMod SHIFT, c, exec, code --enable-features=UseOzonePlatform --ozone-platform=wayland  --enable-wayland-ime 

            bind = $mainMod, L, togglefloating,
            bind = $mainMod, V, pseudo, # dwindle
            bind = $mainMod, W, swapactiveworkspaces, eDP-1 HDMI-A-1
            bind = $mainMod, T, togglesplit, # dwindle
            bind = $mainMod, G, togglegroup, # dwindle
            bind = $mainMod, M ,layoutmsg, swapwithmaster
            bind = $mainMod SHIFT, M, layoutmsg, focusmaster
            bind = $mainMod, space, fullscreen, # dwindle

            # Move focus with mainMod + arrow keys
            bind = $mainMod, B, movefocus, l
            bind = $mainMod, F, movefocus, r
            bind = $mainMod, P, movefocus, u
            bind = $mainMod, N, movefocus, d
            bind = $mainMod, 25, focusmonitor, l
            bind = $mainMod, 26, focusmonitor, r

            # Scroll through existing workspaces with mainMod + scroll
            bind = $mainMod SHIFT, mouse_down, workspace, e+1
            bind = $mainMod SHIFT, mouse_up, workspace, e-1

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
