# Dvorak Programmer Layout - Developer-Optimized Hyprland Keybindings
# Philosophy: Programmer-focused navigation optimized for development workflow
# - Combines Dvorak efficiency with developer muscle memory
# - h/t/n/s: window focus movement (left/down/up/right) - based on Dvorak home row
# - Easy access to symbols and coding operations
# - Optimized for programming tasks and symbol usage

{ pkgs, ... }:
{
  wayland = {
    windowManager = {
      hyprland = {
        settings = {
          input = {
            kb_variant = "dvp";
            kb_model = "dvorak-programmer";
          };

          bind = [
            # Programmer-focused navigation (using DVP home row positions)
            "$mod, h, movefocus, l" # left (h is leftmost home key in DVP)
            "$mod, t, movefocus, d" # down (t is down from home row)
            "$mod, n, movefocus, u" # up (n is up from home row)
            "$mod, s, movefocus, r" # right (s is rightmost home key in DVP)

            # Developer window movement
            "$mod SHIFT, h, movewindow, l"
            "$mod SHIFT, t, movewindow, d"
            "$mod SHIFT, n, movewindow, u"
            "$mod SHIFT, s, movewindow, r"

            # Programming-optimized resizing
            "$mod CTRL, h, resizeactive, -50 0"
            "$mod CTRL, t, resizeactive, 0 50"
            "$mod CTRL, n, resizeactive, 0 -50"
            "$mod CTRL, s, resizeactive, 50 0"

            # Workspace switching (DVP number row: &[{}(=*)+])
            "$mod, ampersand, moveworkspacetomonitor, 1 current" # &
            "$mod, ampersand, workspace, 1"
            "$mod, bracketleft, moveworkspacetomonitor, 2 current" # [
            "$mod, bracketleft, workspace, 2"
            "$mod, braceleft, moveworkspacetomonitor, 3 current" # {
            "$mod, braceleft, workspace, 3"
            "$mod, braceright, moveworkspacetomonitor, 4 current" # }
            "$mod, braceright, workspace, 4"
            "$mod, parenleft, moveworkspacetomonitor, 5 current" # (
            "$mod, parenleft, workspace, 5"
            "$mod, equal, moveworkspacetomonitor, 6 current" # =
            "$mod, equal, workspace, 6"
            "$mod, asterisk, moveworkspacetomonitor, 7 current" # *
            "$mod, asterisk, workspace, 7"
            "$mod, parenright, moveworkspacetomonitor, 8 current" # )
            "$mod, parenright, workspace, 8"
            "$mod, plus, moveworkspacetomonitor, 9 current" # +
            "$mod, plus, workspace, 9"
            "$mod, bracketright, moveworkspacetomonitor, 10 current" # ]
            "$mod, bracketright, workspace, 10"

            # Move windows to workspaces (DVP symbols)
            "$mod SHIFT, ampersand, movetoworkspace, 1"
            "$mod SHIFT, bracketleft, movetoworkspace, 2"
            "$mod SHIFT, braceleft, movetoworkspace, 3"
            "$mod SHIFT, braceright, movetoworkspace, 4"
            "$mod SHIFT, parenleft, movetoworkspace, 5"
            "$mod SHIFT, equal, movetoworkspace, 6"
            "$mod SHIFT, asterisk, movetoworkspace, 7"
            "$mod SHIFT, parenright, movetoworkspace, 8"
            "$mod SHIFT, plus, movetoworkspace, 9"
            "$mod SHIFT, bracketright, movetoworkspace, 10"

            # Developer-focused commands
            "$mod, q, killactive" # quit (easy to reach)
            "$mod SHIFT, q, exit" # quit WM
            "$mod, return, exec, ${pkgs.alacritty}/bin/alacritty" # terminal (essential for devs)
            "$mod, x, exec, ${pkgs.albert}/bin/albert toggle" # M-x (execute command) - ergonomic on Dvorak

            # Programming splits and layouts
            "$mod, minus, layoutmsg, togglesplit" # horizontal split (-)
            "$mod, backslash, togglesplit" # vertical split (\)
            "$mod, v, pseudo" # pseudo-tiling
            "$mod, w, swapactiveworkspaces, eDP-1 HDMI-A-1" # workspace swap

            # Developer workflow
            "$mod, d, cyclenext" # cycle through windows (development)
            "$mod SHIFT, d, cyclenext, prev" # reverse cycle
            "$mod, c, togglefloating" # code/float toggle
            "$mod, r, layoutmsg, swapwithmaster" # refactor/refresh
            "$mod, g, togglegroup" # group (like git)
            "$mod, m, layoutmsg, focusmaster" # master (main window)
            "$mod SHIFT, m, layoutmsg, swapwithmaster" # swap with master
            "$mod, u, focusurgentorlast" # urgent (debugging)

            # Multi-monitor development setup
            "$mod, e, focusmonitor, l" # external monitor left
            "$mod, i, focusmonitor, r" # external monitor right (i for right side of keyboard)

            # Developer utilities
            "$mod, tab, cyclenext" # cycle through windows
            "$mod, b, exec, ${pkgs.google-chrome}/bin/google-chrome-stable --ozone-platform=wayland  --enable-wayland-ime" # browser for docs/testing

            # Developer shortcuts
            "$mod, period, exec, ${
              pkgs.writeShellApplication {
                name = "screen-shot";
                text = ''
                  grim -g "$(slurp)" - | wl-copy -o
                '';
              }
            }/bin/screen-shot" # screenshot (.)
            "$mod SHIFT, period, exec, ${
              pkgs.writeShellApplication {
                name = "screen-shot-and-save";
                text = ''
                  TIME=$(date +%Y%m%d-%H%M%S)_screenshot
                  FILE_PATH=~/Pictures/"$TIME".png
                  grim -g "$(slurp)" "$FILE_PATH"
                  echo -n "$FILE_PATH" | wl-copy
                '';
              }
            }/bin/screen-shot-and-save" # save screenshot
          ];
        };
      };
    };
  };
}
