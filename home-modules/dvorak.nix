# Dvorak Layout - Emacs-inspired Hyprland Keybindings
# Philosophy: Emacs-style navigation and window management
# - b/f/p/n: window focus movement (backward/forward/previous/next)
# - Alt + bfpn: move windows between positions
# - Ctrl + bfpn: resize windows
# - Emacs-like commands (x for kill, C-x for exit, M-x for command, etc.)

{ pkgs, ... }:
{
  wayland = {
    windowManager = {
      hyprland = {
        settings = {
          input = {
            kb_variant = "dvorak";
            kb_model = "";
          };

          bind = [
            # Emacs-like navigation (C-b, C-f, C-p, C-n equivalents)
            "$mod, b, movefocus, l" # backward-char (left)
            "$mod, f, movefocus, r" # forward-char (right)
            "$mod, p, movefocus, u" # previous-line (up)
            "$mod, n, movefocus, d" # next-line (down)

            # Emacs-like window movement with Alt (M-)
            "$mod ALT, b, movewindow, l"
            "$mod ALT, f, movewindow, r"
            "$mod ALT, p, movewindow, u"
            "$mod ALT, n, movewindow, d"

            # Emacs-like resizing (using C-x prefix metaphor)
            "$mod CTRL, b, resizeactive, -50 0"
            "$mod CTRL, f, resizeactive, 50 0"
            "$mod CTRL, p, resizeactive, 0 -50"
            "$mod CTRL, n, resizeactive, 0 50"

            # Workspace switching (standard numbers)
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

            # Emacs-like window management
            "$mod, x, killactive" # C-x k (kill-buffer)
            "$mod CTRL, x, exit" # C-x C-c (exit emacs)
            "$mod, return, exec, ${pkgs.alacritty}/bin/alacritty" # terminal
            "$mod ALT, x, exec, ${pkgs.albert}/bin/albert toggle" # M-x (execute command) - ergonomic on Dvorak

            # Emacs window operations
            "$mod CTRL, f, togglefloating" # C-f for forward/float
            "$mod, v, pseudo" # pseudo-tiling
            "$mod ALT, v, togglesplit" # M-v for split toggle
            "$mod, w, swapactiveworkspaces, eDP-1 HDMI-A-1" # swap workspaces
            "$mod, g, togglegroup" # group toggle
            "$mod ALT, m, layoutmsg, swapwithmaster" # M-m for master
            "$mod CTRL, m, layoutmsg, focusmaster" # C-m for focus master

            # Emacs-like monitor navigation (using C-x o concept)
            "$mod CTRL, o, focusmonitor, l" # C-x o (other window)
            "$mod ALT, o, focusmonitor, r" # M-o (other direction)

            # Emacs-inspired utilities
            "$mod, u, focusurgentorlast" # urgent focus
            "$mod, tab, cyclenext" # tab through buffers
            "$mod, e, exec, ${pkgs.google-chrome}/bin/google-chrome-stable --ozone-platform=wayland  --enable-wayland-ime" # external browser

            # Screenshot with emacs-like bindings
            "$mod ALT, a, exec, ${
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
            "$mod CTRL, x, exit" # C-x C-c (save-buffers-kill-terminal)
            "$mod SHIFT, x, exec, ${pkgs.albert}/bin/albert toggle" # M-x (execute-command)

            # Emacs-like splits (C-x 2, C-x 3)
            "$mod, 2, layoutmsg, togglesplit" # C-x 2 (split horizontal)
            "$mod, 3, togglesplit" # C-x 3 (split vertical)

            # Emacs-like buffer navigation
            "$mod, o, cyclenext" # C-x o (other-window)
            "$mod SHIFT, o, cyclenext, prev" # C-x O (other-window reverse)

            # Additional Emacs-inspired bindings
            "$mod, g, exec, hyprctl dispatch cancelexitworkspace" # C-g (keyboard-quit)
            "$mod, space, fullscreen" # Mark/select mode
            "$mod, s, exec, grim -g \"$(slurp)\" - | wl-copy -o" # C-s (search/save)
            "$mod, r, layoutmsg, swapwithmaster" # refresh/recenter
            "$mod, l, togglefloating" # C-l (recenter)
            "$mod, u, focusurgentorlast" # universal-argument
            "$mod, m, layoutmsg, focusmaster" # M-< (beginning-of-buffer)
            "$mod, t, togglegroup" # transpose
          ];
        };
      };
    };
  };
}
