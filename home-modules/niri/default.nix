{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:
let
  noctalia = "${pkgs.noctalia-shell}/bin/noctalia-shell";

  # niri's `spawn-at-startup` runs while niri is still initialising — its IPC
  # server (and therefore NIRI_SOCKET) is not yet exported into spawned
  # children's environments. Without NIRI_SOCKET, noctalia falls back to the
  # generic ext-workspace backend ("no recognized compositor env" in its
  # logs) and the bar surface fails to show on niri's outputs. Wrap the
  # noctalia spawn in a small shell that polls for niri's IPC socket
  # (~5 s timeout) and exports NIRI_SOCKET before exec'ing noctalia-shell.
  noctaliaSpawn = pkgs.writeShellScript "noctalia-spawn" ''
    if [ -z "''${NIRI_SOCKET:-}" ]; then
      for _ in $(${pkgs.coreutils}/bin/seq 1 50); do
        sock=$(${pkgs.coreutils}/bin/ls -t "''${XDG_RUNTIME_DIR}"/niri.wayland-*.sock 2>/dev/null | ${pkgs.coreutils}/bin/head -n1)
        if [ -n "$sock" ]; then
          export NIRI_SOCKET="$sock"
          break
        fi
        ${pkgs.coreutils}/bin/sleep 0.1
      done
    fi
    exec ${noctalia}
  '';
in
{
  home = lib.mkIf pkgs.stdenv.isLinux {
    packages = with pkgs; [
      # Core Wayland utilities
      grim
      slurp
      hyprpicker # wlr-screencopy color picker
      brightnessctl # fallback brightness (Noctalia IPC is preferred)
      wl-clipboard # clipboard CLI
      wf-recorder
      wev # wayland event viewer
      waypipe

      # Shell
      noctalia-shell # bar + notifications + OSD + launcher + lock + wallpaper + nightlight

      # Compositor glue Noctalia doesn't own
      xwayland-satellite
      polkit_gnome
    ];

    sessionVariables = {
      NIX_LD = toString (
        pkgs.runCommand "ld.so" { } ''
          ln -s "$(cat '${pkgs.stdenv.cc}/nix-support/dynamic-linker')" $out
        ''
      );
      NIXOS_OZONE_WL = "1";
      XDG_SESSION_TYPE = "wayland";
      XDG_SESSION_DESKTOP = "niri";
      XDG_CURRENT_DESKTOP = "niri";
    };
  };

  programs.niri.settings = {
    # Environment pushed to niri itself and imported into systemd --user
    # via niri-session. Needed because tuigreet --cmd niri-session exec's
    # directly and never sources hm-session-vars.sh, so the IM env vars
    # written by i18n.inputMethod.fcitx5 would otherwise be missing.
    # GTK_IM_MODULE/XMODIFIERS are intentionally omitted: on Wayland,
    # GTK3/4 reach fcitx5 natively via text-input-v3, and XMODIFIERS is
    # X11-only. Setting GTK_IM_MODULE=fcitx triggers fcitx5's Wayland
    # Diagnose warning about double input handling.
    environment = {
      QT_IM_MODULE = "fcitx";
      SDL_IM_MODULE = "fcitx";
      GLFW_IM_MODULE = "ibus";
    };

    outputs = {
      "eDP-1" = {
        scale = 1.5;
      };
      "HDMI-A-1" = {
        scale = 1.0;
      };
    };

    input = {
      keyboard.xkb = {
        layout = "us";
        options = "caps:ctrl_modifier";
      };
      touchpad = {
        tap = true;
        natural-scroll = false;
      };
      mouse = {
        natural-scroll = false;
      };
    };

    layout = {
      gaps = 8;
      focus-ring = {
        enable = true;
        width = 2;
        active.color = "#cba6f7";
        inactive.color = "#44475a";
      };
      border.enable = false;
    };

    window-rules = [
      {
        draw-border-with-background = false;
        geometry-corner-radius = {
          top-left = 6.0;
          top-right = 6.0;
          bottom-left = 6.0;
          bottom-right = 6.0;
        };
        clip-to-geometry = true;
      }
    ];

    spawn-at-startup = [
      # Desktop shell — owns bar, notifications, OSD, launcher, lock,
      # wallpaper, night light, idle management. Wrapper exports
      # NIRI_SOCKET (see `noctaliaSpawn` above for context).
      { command = [ "${noctaliaSpawn}" ]; }

      # Polkit auth agent (GUI password prompts)
      { command = [ "${pkgs.polkit_gnome}/libexec/polkit-gnome-authentication-agent-1" ]; }

      # X11 compatibility shim
      { command = [ "${pkgs.xwayland-satellite}/bin/xwayland-satellite" ]; }

      # Chinese input method — niri doesn't process XDG autostart, so
      # home-manager's .config/autostart/org.fcitx.Fcitx5.desktop never fires.
      {
        command = [
          "fcitx5"
          "-d"
          "--replace"
        ];
      }

      # User-space autostart
      { command = [ "${pkgs.netbird-ui}/bin/netbird-ui" ]; }
      {
        command = [
          "dropbox"
          "start"
        ];
      }
    ];

    # Keybindings mirror niri's upstream defaults.
    # Spawn binds point at Noctalia IPC so the shell owns OSDs + UX.
    binds = with config.lib.niri.actions; {
      "Mod+Shift+Slash".action = show-hotkey-overlay;

      "Mod+T" = {
        action = spawn "${pkgs.ghostty}/bin/ghostty";
        hotkey-overlay.title = "Terminal (ghostty)";
      };
      "Mod+D" = {
        action = spawn noctalia "ipc" "call" "launcher" "toggle";
        hotkey-overlay.title = "Launcher";
      };
      "Mod+Y" = {
        action = spawn noctalia "ipc" "call" "launcher" "clipboard";
        hotkey-overlay.title = "Clipboard History";
      };
      "Super+Alt+L" = {
        action = spawn noctalia "ipc" "call" "lockScreen" "lock";
        hotkey-overlay.title = "Lock Screen";
      };
      "Mod+N" = {
        action = spawn noctalia "ipc" "call" "notifications" "toggleHistory";
        hotkey-overlay.title = "Notifications History";
      };
      "Mod+Shift+N" = {
        action = spawn noctalia "ipc" "call" "notifications" "toggleDND";
        hotkey-overlay.title = "Toggle Do Not Disturb";
      };
      "Mod+Shift+S" = {
        action = spawn noctalia "ipc" "call" "settings" "toggle";
        hotkey-overlay.title = "Shell Settings";
      };
      "Mod+M" = {
        action = spawn noctalia "ipc" "call" "controlCenter" "toggle";
        hotkey-overlay.title = "Control Center";
      };

      # Volume — routed through Noctalia so OSD appears
      "XF86AudioRaiseVolume" = {
        action = spawn noctalia "ipc" "call" "volume" "increase";
        allow-when-locked = true;
      };
      "XF86AudioLowerVolume" = {
        action = spawn noctalia "ipc" "call" "volume" "decrease";
        allow-when-locked = true;
      };
      "XF86AudioMute" = {
        action = spawn noctalia "ipc" "call" "volume" "muteOutput";
        allow-when-locked = true;
      };
      "XF86AudioMicMute" = {
        action = spawn noctalia "ipc" "call" "volume" "muteInput";
        allow-when-locked = true;
      };

      # Brightness — routed through Noctalia
      "XF86MonBrightnessUp" = {
        action = spawn noctalia "ipc" "call" "brightness" "increase";
        allow-when-locked = true;
      };
      "XF86MonBrightnessDown" = {
        action = spawn noctalia "ipc" "call" "brightness" "decrease";
        allow-when-locked = true;
      };

      "Mod+Q".action = close-window;

      # Focus navigation
      "Mod+Left".action = focus-column-left;
      "Mod+Down".action = focus-window-down;
      "Mod+Up".action = focus-window-up;
      "Mod+Right".action = focus-column-right;
      "Mod+H".action = focus-column-left;
      "Mod+J".action = focus-window-down;
      "Mod+K".action = focus-window-up;
      "Mod+L".action = focus-column-right;

      # Move columns / windows
      "Mod+Ctrl+Left".action = move-column-left;
      "Mod+Ctrl+Down".action = move-window-down;
      "Mod+Ctrl+Up".action = move-window-up;
      "Mod+Ctrl+Right".action = move-column-right;
      "Mod+Ctrl+H".action = move-column-left;
      "Mod+Ctrl+J".action = move-window-down;
      "Mod+Ctrl+K".action = move-window-up;
      "Mod+Ctrl+L".action = move-column-right;

      "Mod+Home".action = focus-column-first;
      "Mod+End".action = focus-column-last;
      "Mod+Ctrl+Home".action = move-column-to-first;
      "Mod+Ctrl+End".action = move-column-to-last;

      # Monitor focus
      "Mod+Shift+Left".action = focus-monitor-left;
      "Mod+Shift+Down".action = focus-monitor-down;
      "Mod+Shift+Up".action = focus-monitor-up;
      "Mod+Shift+Right".action = focus-monitor-right;
      "Mod+Shift+H".action = focus-monitor-left;
      "Mod+Shift+J".action = focus-monitor-down;
      "Mod+Shift+K".action = focus-monitor-up;
      "Mod+Shift+L".action = focus-monitor-right;

      # Move column to monitor
      "Mod+Shift+Ctrl+Left".action = move-column-to-monitor-left;
      "Mod+Shift+Ctrl+Down".action = move-column-to-monitor-down;
      "Mod+Shift+Ctrl+Up".action = move-column-to-monitor-up;
      "Mod+Shift+Ctrl+Right".action = move-column-to-monitor-right;
      "Mod+Shift+Ctrl+H".action = move-column-to-monitor-left;
      "Mod+Shift+Ctrl+J".action = move-column-to-monitor-down;
      "Mod+Shift+Ctrl+K".action = move-column-to-monitor-up;
      "Mod+Shift+Ctrl+L".action = move-column-to-monitor-right;

      # Move whole workspace to monitor
      "Mod+Shift+Alt+H".action = move-workspace-to-monitor-left;
      "Mod+Shift+Alt+J".action = move-workspace-to-monitor-down;
      "Mod+Shift+Alt+K".action = move-workspace-to-monitor-up;
      "Mod+Shift+Alt+L".action = move-workspace-to-monitor-right;

      # Workspaces
      "Mod+Page_Down".action = focus-workspace-down;
      "Mod+Page_Up".action = focus-workspace-up;
      "Mod+U".action = focus-workspace-down;
      "Mod+I".action = focus-workspace-up;
      "Mod+Ctrl+Page_Down".action.move-column-to-workspace-down = { };
      "Mod+Ctrl+Page_Up".action.move-column-to-workspace-up = { };
      "Mod+Ctrl+U".action.move-column-to-workspace-down = { };
      "Mod+Ctrl+I".action.move-column-to-workspace-up = { };

      "Mod+Shift+Page_Down".action = move-workspace-down;
      "Mod+Shift+Page_Up".action = move-workspace-up;
      "Mod+Shift+U".action = move-workspace-down;
      "Mod+Shift+I".action = move-workspace-up;

      "Mod+1".action = focus-workspace 1;
      "Mod+2".action = focus-workspace 2;
      "Mod+3".action = focus-workspace 3;
      "Mod+4".action = focus-workspace 4;
      "Mod+5".action = focus-workspace 5;
      "Mod+6".action = focus-workspace 6;
      "Mod+7".action = focus-workspace 7;
      "Mod+8".action = focus-workspace 8;
      "Mod+9".action = focus-workspace 9;

      "Mod+Ctrl+1".action.move-column-to-workspace = 1;
      "Mod+Ctrl+2".action.move-column-to-workspace = 2;
      "Mod+Ctrl+3".action.move-column-to-workspace = 3;
      "Mod+Ctrl+4".action.move-column-to-workspace = 4;
      "Mod+Ctrl+5".action.move-column-to-workspace = 5;
      "Mod+Ctrl+6".action.move-column-to-workspace = 6;
      "Mod+Ctrl+7".action.move-column-to-workspace = 7;
      "Mod+Ctrl+8".action.move-column-to-workspace = 8;
      "Mod+Ctrl+9".action.move-column-to-workspace = 9;

      # Column shaping
      "Mod+Comma".action = consume-window-into-column;
      "Mod+Period".action = expel-window-from-column;
      "Mod+BracketLeft".action = consume-or-expel-window-left;
      "Mod+BracketRight".action = consume-or-expel-window-right;

      # Floating / tiling
      "Mod+V".action = toggle-window-floating;
      "Mod+Shift+V".action = switch-focus-between-floating-and-tiling;

      # Overview (moved off Mod+W to free it for tabbed-column toggle)
      "Mod+O".action = toggle-overview;

      # Tabbed-column display — Mod+W toggles the focused column between
      # tiled and tabbed rendering. Mod+Up/Down already navigate within
      # a tabbed column, Mod+BracketLeft/Right already consume/expel
      # windows into/out of the column (i.e. add/remove tabs).
      "Mod+W".action = toggle-column-tabbed-display;

      "Mod+R".action = switch-preset-column-width;
      "Mod+F".action = maximize-column;
      "Mod+Shift+F".action = fullscreen-window;
      "Mod+C".action = center-column;

      "Mod+Minus".action = set-column-width "-10%";
      "Mod+Equal".action = set-column-width "+10%";
      "Mod+Shift+Minus".action = set-window-height "-10%";
      "Mod+Shift+Equal".action = set-window-height "+10%";

      # Screenshots (niri built-in)
      "Print".action.screenshot = { };
      "Ctrl+Print".action.screenshot-screen = { };
      "Alt+Print".action.screenshot-window = { };

      "Mod+Shift+E".action = quit;
      "Mod+Shift+P".action = power-off-monitors;
    };
  };

  # --- xray background effect ---------------------------------------
  # niri 25.11 supports `background-effect { xray true }` in window- and
  # layer-rules, but niri-flake's typed settings schema doesn't expose
  # it yet (no open PR at sodiboo/niri-flake as of 2026-04). Until that
  # lands, append the xray rule as raw KDL to the config file niri-flake
  # installs, and re-validate against the real niri binary at build time.
  #
  # Removing this block is as simple as removing it; `programs.niri.settings`
  # above is untouched.
  xdg.configFile.niri-config.source = lib.mkForce (
    inputs.niri.lib.internal.validated-config-for pkgs config.programs.niri.package (
      config.programs.niri.finalConfig
      + ''

        // Make all windows "see through" to the wallpaper, ignoring windows
        // below. Niri auto-enables this when other background effects are
        // active, but we want it on every window regardless so transparent
        // terminals / editors show the wallpaper.
        window-rule {
            background-effect {
                xray true
            }
        }
      ''
    )
  );
}
