# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ pkgs, lib, ... }: {
  home = lib.mkIf pkgs.stdenv.isLinux {
    sessionVariables = {
      NIX_LD = toString (pkgs.runCommand "ld.so" { } ''
        ln -s "$(cat '${pkgs.stdenv.cc}/nix-support/dynamic-linker')" $out
      '');
      INPUT_METHOD = "fcitx";
      XIM_SERVERS = "fcitx";
      NIXOS_OZONE_WL = "1";
      # WLR_NO_HARDWARE_CURSORS = "1";
      XDG_SESSION_TYPE = "wayland";
      # LSP_USE_PLISTS = "true";
      NIX_SSHOPTS = "-Y";
    };
    file = {
      ".config/hypr/hyprpaper.conf" = {
        text = ''
          preload = ~/Dropbox/WallPaper/dark/girls_with_guns_anime_girl_butterfly_101109_1920x1080.jpg
          preload = ~/Dropbox/WallPaper/dark/vertical/1.jpg
          preload = ~/Dropbox/WallPaper/dark/vertical/2.jpg
          wallpaper = eDP-1,~/Dropbox/WallPaper/dark/girls_with_guns_anime_girl_butterfly_101109_1920x1080.jpg
          wallpaper = HDMI-A-1,~/Dropbox/WallPaper/dark/vertical/1.jpg
          ipc = on
        '';
        executable = false;
      };
    };
  };
  wayland = {
    windowManager = {
      hyprland = let
        clean-up-after-start = pkgs.writeShellApplication {
          name = "clean-up-after-start.sh";
          runtimeInputs = [ pkgs.hyprland ];
          text = ''
            sleep 10
            hyprctl keyword windowrule "workspace unset,brave"
          '';
        };
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

        workspace = pkgs.writeShellApplication {
          name = "workspace.sh";
          runtimeInputs = [ pkgs.hyprland pkgs.gnugrep pkgs.gawk ];
          text = ''
            monitors=/tmp/hypr/monitors_temp
            hyprctl monitors > $monitors

            if [[ -z $1 ]]; then
              workspace=$(grep -B 4 "focused: no" "$monitors" | awk 'NR==1 {print $3}')
            else
              workspace=$1
            fi

            activemonitor=$(grep -B 7 "focused: yes" "$monitors" | awk 'NR==1 {print $2}')
            passivemonitor=$(grep  -B 3 "($workspace)" "$monitors" | awk 'NR==1 {print $2}')
            #activews=$(grep -A 2 "$activemonitor" "$monitors" | awk 'NR==3 {print $1}' RS='(' FS=')')
            passivews=$(grep -A 2 "$passivemonitor" "$monitors" | awk 'NR==4 {print $1}' RS='(' FS=')')

            if [[ $workspace -eq $passivews ]] && [[ $activemonitor != "$passivemonitor" ]]; then
              hyprctl dispatch swapactiveworkspaces "$activemonitor" "$passivemonitor"
              echo "$activemonitor" "$passivemonitor"
            else
              hyprctl dispatch moveworkspacetomonitor "$workspace $activemonitor" && hyprctl dispatch workspace "$workspace"
            fi
          '';
        };
      in {
        enable = true;
        xwayland = {
          enable = true;
          # hidpi = true;
        };

        extraConfig = lib.mkDefault ''
          # This is an example Hyprland config file.
          #
          # Refer to the wiki for more information.

          #
          # Please note not all available settings / options are set here.
          # For a full list, see the wiki
          #

          # See https://wiki.hyprland.org/Configuring/Monitors/
          monitor=,highrr,auto,auto
          monitor=HDMI-A-1,preferred,auto,1,transform,1
          workspace = , 1
          workspace=HDMI-A-1,1

          # if not work check monitor driver
          monitor=DP-1,3840x2160@60,1920x0,1.5
          # monitor=DP-1,transform,1
          # workspace = eDP-2, 1
          # workspace=DP-1,10

          # Source a file (multi-file configs)
          # source = ~/.config/hypr/myColors.conf

          # For all categories, see https://wiki.hyprland.org/Configuring/Variables/
          input {
          kb_layout = us
          # kb_variant = dvp
          # kb_model = dvorak-programmer

          # kb_options = caps:ctrl_modifier
          kb_options = ctrl:nocaps

          kb_rules =
          repeat_rate = 60
          repeat_delay = 180
          # left_handed = 1
          follow_mouse = 1
          # unscale XWayland
          xwayland {
            
          }
          touchpad {
          natural_scroll = no
          }
          sensitivity = 1.0 # -1.0 - 1.0, 0 means no modification.
          }

          general {
          # See https://wiki.hyprland.org/Configuring/Variables/ for more
          # no_cursor_warps = 1
          gaps_in = 5
          gaps_out = 15
          border_size = 2
          col.active_border = rgba(1affffee)
          col.inactive_border = rgba(595959aa)

          layout = dwindle
          }

          decoration {
          # See https://wiki.hyprland.org/Configuring/Variables/ for more

          rounding = 10

          drop_shadow = yes
          shadow_range = 4
          shadow_render_power = 3
          col.shadow = rgba(1a1a1aee)
          }

          animations {
          enabled = yes

          # Some default animations, see https://wiki.hyprland.org/Configuring/Animations/ for more

          bezier = myBezier, 0.05, 0.9, 0.1, 1.05

          animation = windows, 1, 7, myBezier
          animation = windowsOut, 1, 7, default, popin 80%
          animation = border, 1, 10, default
          animation = fade, 1, 7, default
          animation = workspaces, 1, 6, default
          }

          dwindle {
          # See https://wiki.hyprland.org/Configuring/Dwindle-Layout/ for more
          pseudotile = yes # master switch for pseudotiling. Enabling is bound to mainMod + P in the keybinds section below
          preserve_split = yes # you probably want this
          }

          master {
          # See https://wiki.hyprland.org/Configuring/Master-Layout/ for more
          new_is_master = true
          }

          gestures {
          # See https://wiki.hyprland.org/Configuring/Variables/ for more
          workspace_swipe = on
          }
          # Example windowrule v1
          # windowrule = float, ^(kitty)$
          # Example windowrule v2
          # windowrulev2 = float,class:^(kitty)$,title:^(kitty)$
          # See https://wiki.hyprland.org/Configuring/Window-Rules/ for more

          # See https://wiki.hyprland.org/Configuring/Keywords/ for more
          $mainMod = SUPER

          # Example binds, see https://wiki.hyprland.org/Configuring/Binds/ for more
          bind = $mainMod, return, exec, alacritty
          bind = $mainMod, c, killactive,
          bind = $mainMod SHIFT, Q, exit,
          bind = $mainMod SHIFT, A, exec, ${screen-shot-and-save}/bin/screen-shot-and-save.sh
          bind = $mainMod SHIFT, S, exec, ${screen-shot}/bin/screen-shot.sh
          bind = $mainMod, E, exec, brave --ozone-platform=wayland  --enable-wayland-ime 
          # bind = $mainMod SHIFT, c, exec, code --enable-features=UseOzonePlatform --ozone-platform=wayland  --enable-wayland-ime 
          bind = $mainMod, X, exec, wofi --show drun -I -G
          bind = $mainMod, L, togglefloating,
          bind = $mainMod, V, pseudo, # dwindle
          bind = $mainMod, W, swapactiveworkspaces, eDP-1 HDMI-A-1
          bind = $mainMod, T, togglesplit, # dwindle
          bind = $mainMod, G, togglegroup, # dwindle
          bind = $mainMod, O, toggleopaque,
          bind = $mainMod, M ,layoutmsg, swapwithmaster
          bind = $mainMod SHIFT, M, layoutmsg, focusmaster
          bind = $mainMod, space, fullscreen, # dwindle

          # Move focus with mainMod + arrow keys
          bind = $mainMod, B, movefocus, l
          bind = $mainMod, F, movefocus, r
          bind = $mainMod, P, movefocus, u
          bind = $mainMod, N, movefocus, d

          # workspaces
          # binds mod + [shift +] {1..10} to [move to] ws {1..10}
          ${builtins.concatStringsSep "\n" (builtins.genList (x:
            let
              ws = let c = (x + 1) / 10;
              in builtins.toString (x + 1 - (c * 10));
            in ''
              # bind = $mainMod, ${ws}, workspace, ${toString (x + 1)}
              bind=$mainMod,${ws},exec, ${workspace}/bin/workspace.sh ${
                toString (x + 1)
              }
              bind = $mainMod SHIFT, ${ws}, movetoworkspace, ${toString (x + 1)}
            '') 10)}
          # cycle monitors

          bind = $mainMod, 25, focusmonitor, l
          bind = $mainMod, 26, focusmonitor, r

          # Scroll through existing workspaces with mainMod + scroll
          bind = $mainMod SHIFT, mouse_down, workspace, e+1
          bind = $mainMod SHIFT, mouse_up, workspace, e-1

          # Move/resize wndows with mainMod + LMBi/RMB and dragging
          bindm = $mainMod SHIFT, mouse:272, movewindow
          bindm = $mainMod, mouse:273, resizewindow

          # volumn
          binde=, XF86AudioRaiseVolume, exec, amixer sset Master 5%+
          binde=, XF86AudioLowerVolume, exec, amixer sset Master 5%-
          bind=, XF86AudioMute, exec, amixer -D pipewire set Master 1+ toggle

          # brightness
          binde=, XF86MonBrightnessUp, exec, brightnessctl s +5%
          binde=, XF86MonBrightnessDown, exec, brightnessctl s 5%-

          # window resize
          bind = $mainMod, S, submap, resize
          submap = resize
          binde = , right, resizeactive, 10 0
          binde = , left, resizeactive, -10 0
          binde = , up, resizeactive, 0 -10
          binde = , down, resizeactive, 0 10
          bind = , escape, submap, reset
          submap = reset

          exec-once = hyprpaper

          windowrule=workspace 1 silent,alacritty
          # windowrule=workspace 3 silent,brave
          windowrule=float,noblur,noshadow,noborder,pseudo,dimaround,albert

          exec-once=dropbox
          exec-once=netbird-ui

          # exec-once=${clean-up-after-start}/bin/clean-up-after-start.sh

          windowrule = opacity 0.9 0.95,Alacritty
          windowrule = opacity 0.9 0.95,emacs
        '';
      };
    };
  };
  # blur_new_optimizations = true
  # blur_passes = 1
  # blur_size = 3
  # blur = yes

  services = {
    wlsunset = {
      enable = true;
      latitude = "1.352083";
      longitude = "103.819839";
    };
  };

  programs = {
    waybar = {
      enable = true;
      systemd = { enable = true; };
      style = ./waybar.css;
      settings = {
        mainBar = {
          layer = "top";
          position = "top";
          # height = 30;
          spacing = 2;
          margin-bottom = -15;

          output = [ "eDP-1" "eDP-2" "HDMI-A-1" ];
          modules-left = [ "hyprland/workspaces" ];
          modules-center = [ "hyprland/window" ];
          modules-right = [
            "custom/btc"
            # "mpd"
            "wlr/taskbar"
            "cpu"
            "memory"
            "temperature"
            "pulseaudio"
            "network"
            "backlight"
            "clock"
            "battery"
            "tray"
          ];

          "jack" = {
            "format" = "DSP {}%";
            "format-xrun" = "{xruns} xruns";
            "format-disconnected" = "DSP off";
            "realtime" = true;
          };

          "hpprland/workspaces" = {
            format = "{icon}";
            format-active = " {icon} ";
            on-click = "activate";
            on-scroll-up = "hyprctl dispatch workspace e+1";
            on-scroll-down = "hyprctl dispatch workspace e-1";
          };

          "wlr/taskbar" = {
            format = "{icon}";
            icon-size = 14;
            icon-theme = "Numix-Circle";
            tooltip-format = "{title}";
            on-click = "activate";
            on-click-middle = "close";
            ignore-list = [ "Alacritty" ];
          };
          "clock" = {
            "tooltip-format" = ''
              <big>{:%Y %B}</big>
              <tt><small>{calendar}</small></tt>'';
            "interval" = 60;
            "format" = "{:%I:%M}";
          };
          "cpu" = {
            "interval" = 1;
            "format" = "{icon0} {icon1} {icon2} {icon3}";
            "format-icons" = [ "▁" "▂" "▃" "▄" "▅" "▆" "▇" "█" ];
          };
          "memory" = { "format" = "{}% "; };
          "temperature" = {
            "critical-threshold" = 80;
            "format-critical" = "{temperatureC}°C";
            "format" = "";
          };
          backlight = {
            format = "{percent}% {icon}";
            format-icons = [ "" "" "" "" "" "" "" "" "" ];
          };

          battery = {
            states = {
              warning = 50;
              critical = 20;
            };
            format = "{icon}";
            format-charging = "";
            format-plugged = "";
            format-icons = [ "" "" "" "" "" ];
          };
          "battery#bat2" = { bat = "BAT2"; };

          network = {
            format-wifi = "{essid} ({signalStrength}%) ";
            format-ethernet = "{ipaddr}/{cidr} ";
            tooltip-format = "{ifname} via {gwaddr} ";
            format-linked = "{ifname} (No IP) ";
            format-disconnected = "Disconnected ⚠";
            format-alt = "{ifname}: {ipaddr}/{cidr}";
          };

          pulseaudio = {
            format = "{volume}% {icon} {format_source}";
            format-bluetooth = "{volume}% {icon} {format_source}";
            format-bluetooth-muted = " {icon} {format_source}";
            format-muted = " {format_source}";
            format-icons = {
              headphone = "";
              hands-free = "";
              headset = "";
              phone = "";
              portable = "";
              car = "";
              default = [ "" "" "" ];
            };
            on-click = "pavucontrol";
          };

          mpd = {
            format =
              "{stateIcon} {consumeIcon}{randomIcon}{repeatIcon}{singleIcon}{artist} - {album} - {title} ({elapsedTime:%M:%S}/{totalTime:%M:%S}) ⸨{songPosition}|{queueLength}⸩ {volume}% ";
            format-disconnected = "Disconnected ";
            format-stopped =
              "{consumeIcon}{randomIcon}{repeatIcon}{singleIcon}Stopped ";
            unknown-tag = "N/A";
            interval = 2;
            consume-icons = { on = " "; };
            random-icons = {
              off = ''<span color="#f53c3c"></span> '';
              on = " ";
            };
            repeat-icons = { on = " "; };
            single-icons = { on = "1 "; };
            state-icons = {
              paused = "";
              playing = "";
            };
            tooltip-format = "MPD (connected)";
            tooltip-format-disconnected = "MPD (disconnected)";
          };

          "hyprland/window" = {
            format = "👉 {}";
            separate-outputs = false;
          };

          tray = {
            icon-size = 21;
            spacing = 10;
          };

          "custom/btc" = {
            format = " {}";
            interval = 5;
            exec = pkgs.writeShellScript "hello-from-waybar" ''
              ${pkgs.curl}/bin/curl https://blockchain.info/ticker --silent | ${pkgs.jq}/bin/jq .USD.last
            '';
          };
        };
      };
    };
    emacs = { package = pkgs.emacs29-pgtk; };
  };
}
