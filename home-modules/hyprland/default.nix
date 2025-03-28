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
      XDG_SESSION_TYPE = "wayland";
      XDG_SESSION_DESKTOP = "Hyprland";
    };
  };

  wayland.windowManager.hyprland = {
    enable = true;
    xwayland.enable = true;

    settings = {
      monitor = [
        ", highrr, auto, auto"
        "HDMI-A-1, preferred, auto, 1, transform, 1"
      ];

      workspace = [
        "HDMI-A-1, 1"
      ];

      xwayland = {
        force_zero_scaling = true;
      };

      input = {
        kb_layout = "us";
        kb_options = "ctrl:nocaps";
        repeat_rate = 60;
        repeat_delay = 180;
        follow_mouse = 1;
        sensitivity = 1.0;

        touchpad = {
          natural_scroll = false;
        };
      };

      general = {
        gaps_in = 5;
        gaps_out = 15;
        border_size = 2;
        "col.active_border" = "rgba(1affffee)";
        "col.inactive_border" = "rgba(595959aa)";
        layout = "dwindle";

        env = [
          "GDK_SCALE,1.5"
          "XCURSOR_SIZE,32"
        ];
      };

      animations = {
        enabled = true;
        bezier = "myBezier, 0.05, 0.9, 0.1, 1.05";
        animation = [
          "windows, 1, 7, myBezier"
          "windowsOut, 1, 7, default, popin 80%"
          "border, 1, 10, default"
          "fade, 1, 7, default"
          "workspaces, 1, 6, default"
        ];
      };

      dwindle = {
        pseudotile = true;
        preserve_split = true;
      };

      gestures = {
        workspace_swipe = true;
      };

      "$mod" = "SUPER";

      bind = [
        "$mod, return, exec, ${pkgs.alacritty}/bin/alacritty"
        "$mod, X, exec, ${pkgs.albert}/bin/albert toggle"
        "$mod, c, killactive"
        "$mod SHIFT, Q, exit"
        "$mod, L, togglefloating"
        "$mod, V, pseudo"
        "$mod, W, swapactiveworkspaces, eDP-1 HDMI-A-1"
        "$mod, T, togglesplit"
        "$mod, G, togglegroup"
        "$mod, M, layoutmsg, swapwithmaster"
        "$mod SHIFT, M, layoutmsg, focusmaster"
        "$mod, space, fullscreen"
        "$mod, B, movefocus, l"
        "$mod, F, movefocus, r"
        "$mod, P, movefocus, u"
        "$mod, N, movefocus, d"
        "$mod, 25, focusmonitor, l"
        "$mod, 26, focusmonitor, r"
      ];

      binde = [
        ", XF86AudioRaiseVolume, exec, ${pkgs.wireplumber}/bin/wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+"
        ", XF86AudioLowerVolume, exec, ${pkgs.wireplumber}/bin/wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"
        ", XF86MonBrightnessUp, exec, ${pkgs.brightnessctl}/bin/brightnessctl s +5%"
        ", XF86MonBrightnessDown, exec, ${pkgs.brightnessctl}/bin/brightnessctl s 5%-"
      ];

      bindm = [
        "$mod, mouse:272, movewindow"
        "$mod, mouse:273, resizewindow"
        "$mod ALT, mouse:272, resizewindow"
      ];

      exec-once = [
        "${pkgs.albert}/bin/albert"
        "${pkgs.netbird}/bin/netbird-ui"
        "dropbox start"
      ];

      # windowrule = [
      #   "workspace 1 silent, alacritty"
      #   "float,noblur,noshadow,noborder,pseudo,dimaround,albert"
      #   "opacity 0.9 0.95,Alacritty"
      #   "opacity 0.9 0.95,emacs"
      # ];
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
      package = pkgs.emacs30-pgtk;
    };
  };
}
