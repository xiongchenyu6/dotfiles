# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ pkgs, lib, ... }:
{
  home = lib.mkIf pkgs.stdenv.isLinux {
    packages = with pkgs; [
      grim
      slurp
      brightnessctl
      hyprpicker
      wl-clipboard
      wf-recorder # screen recording
      wev # get input events
      waypipe
    ];

    sessionVariables = {
      NIX_LD = toString (
        pkgs.runCommand "ld.so" { } ''
          ln -s "$(cat '${pkgs.stdenv.cc}/nix-support/dynamic-linker')" $out
        ''
      );
      INPUT_METHOD = "fcitx";
      XIM_SERVERS = "fcitx";
      NIXOS_OZONE_WL = "1";
      # WLR_NO_HARDWARE_CURSORS = "1";
      XDG_SESSION_TYPE = "wayland";
      # LSP_USE_PLISTS = "true";
      #NIX_SSHOPTS = "-Y";
      XDG_SESSION_DESKTOP = "Hyprland";
    };
  };
  wayland = {
    windowManager = {
      hyprland =
        let
          clean-up-after-start = pkgs.writeShellApplication {
            name = "clean-up-after-start.sh";
            runtimeInputs = [ pkgs.hyprland ];
            text = ''
              sleep 10
              hyprctl keyword windowrule "workspace unset,microsoft-edge"
            '';
          };

          workspace = pkgs.writeShellApplication {
            name = "workspace.sh";
            runtimeInputs = [
              pkgs.hyprland
              pkgs.gnugrep
              pkgs.gawk
            ];
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
        in
        {
          enable = true;
          xwayland = {
            enable = true;
            # hidpi = true;
          };

          extraConfig = lib.mkDefault ''
            monitor=,highrr,auto,auto

            monitor=HDMI-A-1,preferred,auto,1,transform,1

            workspace=HDMI-A-1,1
            # unscale XWayland
            xwayland {
              force_zero_scaling = true
            }
            input {
            kb_layout = us

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
            # env = QT_QPA_PLATFORMTHEME,qt6ct
            # toolkit-specific scale
            env = GDK_SCALE,1.5
            env = XCURSOR_SIZE,32
            }

            decoration {
            # See https://wiki.hyprland.org/Configuring/Variables/ for more
            rounding = 10                        
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
            bind = $mainMod, X, exec, albert toggle

            # workspaces
            #binds mod + [shift +] {1..10} to [move to] ws {1..10}
            bind = $mainMod, c, killactive,
            bind = $mainMod SHIFT, Q, exit,
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

            ${builtins.concatStringsSep "\n" (
              builtins.genList (
                x:
                let
                  ws =
                    let
                      c = (x + 1) / 10;
                    in
                    builtins.toString (x + 1 - (c * 10));
                in
                ''
                  # bind = $mainMod, ${ws}, workspace, ${toString (x + 1)}
                  bind=$mainMod,${ws},exec, ${workspace}/bin/workspace.sh ${toString (x + 1)}
                  bind = $mainMod SHIFT, ${ws}, movetoworkspace, ${toString (x + 1)}
                ''
              ) 10
            )}
            # cycle monitors

            # Move/resize wndows with mainMod + LMBi/RMB and dragging
            bindm = $mainMod SHIFT, mouse:272, movewindow
            bindm = $mainMod, mouse:273, resizewindow

            # volumn
            binde=, XF86AudioRaiseVolume, exec, wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+
            binde=, XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-
            bind=, XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle
            bind=, XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle

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

            exec-once = albert

            windowrule=workspace 1 silent,alacritty
            windowrule=float,noblur,noshadow,noborder,pseudo,dimaround,albert

            exec-once=netbird-ui

            # exec-once=${clean-up-after-start}/bin/clean-up-after-start.sh

            windowrule = opacity 0.9 0.95,Alacritty
            windowrule = opacity 0.9 0.95,emacs
          '';
        };
    };
  };

  services = {
    hypridle = {
      enable = true;
      settings = {
        general = {
          after_sleep_cmd = "hyprctl dispatch dpms on";
          ignore_dbus_inhibit = false;
          lock_cmd = "hyprlock";
        };

        listener = [
          {
            timeout = 900;
            on-timeout = "hyprlock";
          }
          {
            timeout = 1200;
            on-timeout = "hyprctl dispatch dpms off";
            on-resume = "hyprctl dispatch dpms on";
          }
        ];
      };
    };
    wlsunset = {
      enable = true;
      latitude = "1.352083";
      longitude = "103.819839";
    };
    hyprpaper = {
      enable = true;
      settings = {
        ipc = "on";
        splash = false;
        splash_offset = 2.0;

        preload = [
          "~/Dropbox/WallPaper/dark/girls_with_guns_anime_girl_butterfly_101109_1920x1080.jpg"
          "~/Dropbox/WallPaper/dark/vertical/1.jpg"
          "~/Dropbox/WallPaper/dark/vertical/2.jpg"
        ];

        wallpaper = [
          "eDP-1,~/Dropbox/WallPaper/dark/girls_with_guns_anime_girl_butterfly_101109_1920x1080.jpg"
          "HDMI-A-1,~/Dropbox/WallPaper/dark/vertical/1.jpg"
        ];
      };
    };

  };

  programs = {
    hyprlock = {
      enable = true;
      settings = {
        general = {
          disable_loading_bar = true;
          grace = 300;
          hide_cursor = true;
          no_fade_in = false;
        };

        background = [
          {
            path = "screenshot";
            blur_passes = 3;
            blur_size = 8;
          }
        ];

        input-field = [
          {
            size = "200, 50";
            position = "0, -80";
            monitor = "";
            dots_center = true;
            fade_on_empty = false;
            font_color = "rgb(202, 211, 245)";
            inner_color = "rgb(91, 96, 120)";
            outer_color = "rgb(24, 25, 38)";
            outline_thickness = 5;
            placeholder_text = "Password...";
            shadow_passes = 2;
          }
        ];
      };
    };
    waybar = {
      enable = true;
      systemd = {
        enable = true;
      };
      style = ./waybar.css;
      settings = {
        mainBar = {
          layer = "top";
          position = "top";
          # height = 30;
          spacing = 2;
          margin-bottom = -15;

          output = [
            "eDP-1"
            "eDP-2"
            "HDMI-A-1"
          ];
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
            "format-icons" = [
              "▁"
              "▂"
              "▃"
              "▄"
              "▅"
              "▆"
              "▇"
              "█"
            ];
          };
          "memory" = {
            "format" = "{}% ";
          };
          "temperature" = {
            "critical-threshold" = 80;
            "format-critical" = "{temperatureC}°C";
            "format" = "";
          };
          backlight = {
            format = "{percent}% {icon}";
            format-icons = [
              ""
              ""
              ""
              ""
              ""
              ""
              ""
              ""
              ""
            ];
          };

          battery = {
            states = {
              warning = 50;
              critical = 20;
            };
            format = "{icon}";
            format-charging = "";
            format-plugged = "";
            format-icons = [
              ""
              ""
              ""
              ""
              ""
            ];
          };
          "battery#bat2" = {
            bat = "BAT2";
          };

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
              default = [
                ""
                ""
                ""
              ];
            };
            on-click = "pavucontrol";
          };

          mpd = {
            format = "{stateIcon} {consumeIcon}{randomIcon}{repeatIcon}{singleIcon}{artist} - {album} - {title} ({elapsedTime:%M:%S}/{totalTime:%M:%S}) ⸨{songPosition}|{queueLength}⸩ {volume}% ";
            format-disconnected = "Disconnected ";
            format-stopped = "{consumeIcon}{randomIcon}{repeatIcon}{singleIcon}Stopped ";
            unknown-tag = "N/A";
            interval = 2;
            consume-icons = {
              on = " ";
            };
            random-icons = {
              off = ''<span color="#f53c3c"></span> '';
              on = " ";
            };
            repeat-icons = {
              on = " ";
            };
            single-icons = {
              on = "1 ";
            };
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
    emacs = {
      package = pkgs.emacs29-pgtk;
    };
  };
}
