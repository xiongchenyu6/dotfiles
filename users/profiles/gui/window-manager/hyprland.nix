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
      LSP_USE_PLISTS = "true";
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
          runtimeInputs = [ pkgs.hyprland ];
          text = ''
            grim -g "$(slurp)" - | wl-copy
          '';
        };
      in {
        enable = true;
        xwayland = {
          enable = true;
          hidpi = true;
        };
        #  monitor=HDMI-A-1,3840x2160@60,1920x0,1.5

        extraConfig = ''
          # This is an example Hyprland config file.
          #
          # Refer to the wiki for more information.

          #
          # Please note not all available settings / options are set here.
          # For a full list, see the wiki
          #

          # See https://wiki.hyprland.org/Configuring/Monitors/

          monitor=,preferred,auto,1.5
          monitor=HDMI-A-1,preferred,auto,1.5
          monitor=HDMI-A-1,transform,1
          workspace=HDMI-A-1,1

          # Source a file (multi-file configs)
          # source = ~/.config/hypr/myColors.conf

          # For all categories, see https://wiki.hyprland.org/Configuring/Variables/
          input {
          kb_layout = us
          kb_variant =
          kb_model =
          kb_options = caps:ctrl_modifier

          kb_rules =
          repeat_rate = 60
          repeat_delay = 180

          follow_mouse = 1

          touchpad {
          natural_scroll = no
          }

          sensitivity = 0 # -1.0 - 1.0, 0 means no modification.
          }

          general {
          # See https://wiki.hyprland.org/Configuring/Variables/ for more

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
          blur = yes
          blur_size = 3
          blur_passes = 1
          blur_new_optimizations = true

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

          # Example per-device config
          # See https://wiki.hyprland.org/Configuring/Keywords/#executing for more
          device:epic mouse V1 {
          sensitivity = -0.5
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
          bind = $mainMod SHIFT, C, killactive,
          bind = $mainMod SHIFT, Q, exit,
          bind = $mainMod, B, exec, brave
          bind = $mainMod, P, exec, wofi --show drun -I -G
          bind = $mainMod, F, togglefloating,
          bind = $mainMod, V, pseudo, # dwindle
          bind = $mainMod, W, swapactiveworkspaces, eDP-1 HDMI-A-1
          bind = $mainMod, T, togglesplit, # dwindle
          bind = $mainMod, G, togglegroup, # dwindle
          bind = $mainMod, O, toggleopaque,
          bind = $mainMod, M ,layoutmsg, swapwithmaster
          bind = $mainMod SHIFT, M, layoutmsg, focusmaster
          bind = $mainMod, space, fullscreen, # dwindle
          bind = $mainMod SHIFT, S, exec, ${screen-shot}/bin/screen-shot.sh

          # Move focus with mainMod + arrow keys
          bind = $mainMod, H, movefocus, l
          bind = $mainMod, L, movefocus, r
          bind = $mainMod, K, movefocus, u
          bind = $mainMod, J, movefocus, d

          # Switch workspaces with mainMod + [0-9]
          bind= $mainMod,1,moveworkspacetomonitor,1 current
          bind = $mainMod, 1, workspace, 1
          bind= $mainMod,2,moveworkspacetomonitor,2 current
          bind = $mainMod, 2, workspace, 2
          bind= $mainMod,3,moveworkspacetomonitor,3 current
          bind = $mainMod, 3, workspace, 3
          bind= $mainMod,4,moveworkspacetomonitor,4 current
          bind = $mainMod, 4, workspace, 4
          bind= $mainMod,5,moveworkspacetomonitor,5 current
          bind = $mainMod, 5, workspace, 5
          bind= $mainMod,6,moveworkspacetomonitor,6 current
          bind = $mainMod, 6, workspace, 6
          bind= $mainMod,7,moveworkspacetomonitor,7 current
          bind = $mainMod, 7, workspace, 7
          bind= $mainMod,8,moveworkspacetomonitor,8 current
          bind = $mainMod, 8, workspace, 8
          bind= $mainMod,9,moveworkspacetomonitor,9 current
          bind = $mainMod, 9, workspace, 9
          bind= $mainMod,0,moveworkspacetomonitor,10 current
          bind = $mainMod, 0, workspace, 10

          # Move active window to a workspace with mainMod + SHIFT + [0-9]
          bind = $mainMod SHIFT, 1, movetoworkspace, 1
          bind = $mainMod SHIFT, 2, movetoworkspace, 2
          bind = $mainMod SHIFT, 3, movetoworkspace, 3
          bind = $mainMod SHIFT, 4, movetoworkspace, 4
          bind = $mainMod SHIFT, 5, movetoworkspace, 5
          bind = $mainMod SHIFT, 6, movetoworkspace, 6
          bind = $mainMod SHIFT, 7, movetoworkspace, 7
          bind = $mainMod SHIFT, 8, movetoworkspace, 8
          bind = $mainMod SHIFT, 9, movetoworkspace, 9
          bind = $mainMod SHIFT, 0, movetoworkspace, 10

          # Scroll through existing workspaces with mainMod + scroll
          bind = $mainMod SHIFT, mouse_down, workspace, e+1
          bind = $mainMod SHIFT, mouse_up, workspace, e-1

          # Move/resize windows with mainMod + LMB/RMB and dragging
          bindm = $mainMod SHIFT, mouse:272, movewindow
          bindm = $mainMod, mouse:273, resizewindow

          # volumn
          binde=, XF86AudioRaiseVolume, exec, amixer sset Master 5%+
          binde=, XF86AudioLowerVolume, exec, amixer sset Master 5%-
          bind=, XF86AudioMute, exec, amixer -D pipewire set Master 1+ toggle

          # brightness
          binde=, XF86MonBrightnessUp, exec, brightnessctl s +5%
          binde=, XF86MonBrightnessDown, exec, brightnessctl s 5%-

          # # See https://wiki.hyprland.org/Configuring/Keywords/ for more

          # # Execute your favorite apps at launch
          exec-once = hyprpaper

          windowrule=workspace 1 silent,alacritty
          windowrule=workspace 3 silent,brave

          exec-once=alacritty
          exec-once=brave

          exec-once=${clean-up-after-start}/bin/clean-up-after-start.sh

          windowrule = opacity 0.9 0.95,Alacritty
          windowrule = opacity 0.9 0.95,emacs
        '';
      };
    };
  };

  services = {
    wlsunset = {
      enable = true;
      latitude = "1.352083";
      longitude = "103.819839";
    };
  };
  programs = lib.mkIf pkgs.stdenv.isLinux {
    emacs = { package = pkgs.emacsPgtk; };

    waybar = {
      enable = true;
      package = pkgs.waybar-hyprland;
      systemd = { enable = true; };
      style = ./waybar.css;
      settings = {
        mainBar = {
          layer = "top";
          position = "top";
          # height = 30;
          spacing = 2;
          margin-bottom = -15;

          output = [ "eDP-1" "HDMI-A-1" ];
          modules-left = [ "wlr/workspaces" ];
          modules-center = [ "hyprland/window" ];
          modules-right = [
            "custom/btc"
            "mpd"
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

          "wlr/workspaces" = {
            format = "{icon}";
            format-active = " {icon} ";
            on-click = "activate";
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
            interface = "wlp0s20f3";
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
  };
}
