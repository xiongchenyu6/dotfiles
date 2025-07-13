# QWERTY Layout - Vim-inspired Hyprland Keybindings
# Philosophy: Familiar vim navigation with hjkl keys for movement
# - h/j/k/l: window focus movement (left/down/up/right)
# - Shift + hjkl: move windows between positions
# - Ctrl + hjkl: resize windows
# - Standard vim-like commands (w for close, f for fullscreen, etc.)

{ pkgs, ... }:
{
  wayland = {
    windowManager = {
      hyprland = {
        settings = {
          bind = [
            # Vim-like navigation
            "$mod, h, movefocus, l"
            "$mod, j, movefocus, d"
            "$mod, k, movefocus, u"
            "$mod, l, movefocus, r"

            # Vim-like window movement
            "$mod SHIFT, h, movewindow, l"
            "$mod SHIFT, j, movewindow, d"
            "$mod SHIFT, k, movewindow, u"
            "$mod SHIFT, l, movewindow, r"

            # Vim-like resizing
            "$mod CTRL, h, resizeactive, -50 0"
            "$mod CTRL, j, resizeactive, 0 50"
            "$mod CTRL, k, resizeactive, 0 -50"
            "$mod CTRL, l, resizeactive, 50 0"

            # Workspace switching (vim-like numbers)
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

            # Move windows to workspaces
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

            # Vim-like splits
            "$mod, v, layoutmsg, togglesplit"
            "$mod, s, togglesplit"

            # Vim-like commands
            "$mod, q, killactive" # :q to quit
            "$mod SHIFT, Q, exit" # :q! to force quit
            "$mod, return, exec, alacritty" # terminal
            "$mod, p, exec, ${pkgs.albert}/bin/albert toggle"
            "$mod SHIFT, f, togglefloating" # float toggle
            "$mod, v, pseudo" # pseudo-tiling (vertical split concept)
            "$mod, w, swapactiveworkspaces, eDP-1 HDMI-A-1" # swap workspaces
            "$mod, t, togglegroup" # toggle group (tabs concept)
            "$mod, m, layoutmsg, swapwithmaster" # move to master
            "$mod SHIFT, m, layoutmsg, focusmaster" # focus master

            # Vim-like monitor navigation
            "$mod, e, focusmonitor, l" # previous monitor
            "$mod, r, focusmonitor, r" # next monitor

            # Vim-inspired utilities
            "$mod, u, focusurgentorlast" # urgent/last focus
            "$mod, tab, cyclenext" # cycle through windows
            "$mod SHIFT, tab, cyclenext, prev" # reverse cycle
            "$mod, b, exec, ${pkgs.google-chrome}/bin/google-chrome-stable --ozone-platform=wayland  --enable-wayland-ime" # browser

            # Screenshot with vim-like bindings
            "$mod SHIFT, a, exec, ${
              pkgs.writeShellApplication {
                name = "screen-shot-and-save";
                text = ''
                  TIME=$(date +%Y%m%d-%H%M%S)_screenshot
                  FILE_PATH=~/Pictures/"$TIME".png
                  grim -g "$(slurp)" "$FILE_PATH"
                  echo -n "$FILE_PATH" | wl-copy
                '';
              }
            }/bin/screen-shot-and-save"
            "$mod, a, exec, ${
              pkgs.writeShellApplication {
                name = "screen-shot";
                text = ''
                  grim -g "$(slurp)" - | wl-copy -o
                '';
              }
            }/bin/screen-shot"
          ];
        };
      };
    };
  };
}
