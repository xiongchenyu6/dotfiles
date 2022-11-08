# Edit this configuration file to define what should be installed on

# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{

  imports = [ ./common.nix ];
  home = {
    pointerCursor = {
      name = "Vanilla-DMZ";
      package = pkgs.vanilla-dmz;
      x11 = { enable = true; };
      size = 32;
    };
  };

  # # Packages that should be installed to the user profile.

  # # This value determines the Home Manager release that your
  # # configuration is compatible with. This helps avoid breakage
  # # when a new Home Manager release introduces backwards
  # # incompatible changes.
  # #
  # # You can update Home Manager without changing this value. See
  # # the Home Manager release notes for a list of state version
  # # changes in each release.
  xsession = {
    enable = true;
    initExtra = ''
      ${pkgs.xorg.xset}/bin/xset -b
      ${pkgs.xorg.xset}/bin/xset r rate 180 60
    '';
    windowManager = let old-files-path = ../../../old-files;
    in {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
        config = old-files-path + "/config/xmonad/xmonad.hs";
        extraPackages = haskellPackages: [
          haskellPackages.directory
          haskellPackages.X11
        ];
      };
    };
  };

  gtk = { enable = true; };

  i18n = {
    inputMethod = {
      enabled = "fcitx5";
      fcitx5 = { addons = with pkgs; [ fcitx5-chinese-addons ]; };
    };
  };

  programs = {
    tint2 = { enable = true; };

    chromium = {
      enable = true;
      package = pkgs.brave;
      #google-chrome
    };
    rofi = {
      enable = true;
      theme = "gruvbox-dark";
      font = "Hack Nerd Font 20";
      extraConfig = {
        modi = "drun,ssh,keys,filebrowser";
        kb-primary-paste = "Control+V,Shift+Insert";
        kb-secondary-paste = "Control+v,Insert";
      };
      terminal = "alacritty";
    };
    autorandr = {
      enable = true;
      profiles = {
        office = {
          fingerprint = {
            "eDP-1" =
              "00ffffffffffff0009e54c0900000000121e0104a51e1378036980a7544c9825115356000000010101010101010101010101010101019c3e80c870b03c40302036002ebc1000001a000000fd001e3c4a4a10010a202020202020000000fe00424f452043510a202020202020000000fe004e4531343057554d2d4e36320a000a";
            "HDMI-1" =
              "00ffffffffffff001e6dc15bb37c030004200103803c2278ea40b5ae5142ad260f5054210800d1c061404540010101010101010101014dd000a0f0703e803020350058542100001a000000fd00283c1e873c000a202020202020000000fc004c4720554c54524146494e450a000000ff003230344e54464136513533310a01800203427223090707830100004d01030410121f202261605f5e5d6d030c001000b83c20006001020367d85dc401788003e30f0003e2006ae305c000e6060581606050a36600a0f0701f803020350058542100001a565e00a0a0a029503020350058542100001a023a801871382d40582c450058542100001a00000000000000e2";
          };
          config = {

            "eDP-1" = {
              enable = true;
              crtc = 0;
              primary = true;
              position = "0x0";
              mode = "1920x1200";
              rate = "60.00";
              scale = {
                x = 1.5;
                y = 1.5;
              };
              dpi = 144;
            };

            "HDMI-1" = {
              enable = true;
              crtc = 1;
              primary = false;
              position = "3840x0";
              mode = "3840x2160";
              rate = "60.00";
              rotate = "left";
              scale = {
                x = 1;
                y = 1;
              };
              dpi = 144;
            };
          };
          # hooks.postswitch = ''
          #   polybar-msg cmd restart
          # '';
        };
        home = {
          fingerprint = {
            "eDP-1" =
              "00ffffffffffff0009e54c0900000000121e0104a51e1378036980a7544c9825115356000000010101010101010101010101010101019c3e80c870b03c40302036002ebc1000001a000000fd001e3c4a4a10010a202020202020000000fe00424f452043510a202020202020000000fe004e4531343057554d2d4e36320a000a";
          };
          config = {

            "eDP-1" = {
              enable = true;
              crtc = 0;
              primary = true;
              position = "0x0";
              mode = "1920x1200";
              rate = "60.00";
            };
          };
          # hooks.postswitch = ''
          #   polybar-msg cmd restart
          # '';
        };
      };
    };

    xmobar = {
      enable = true;
      extraConfig = ''
        Config {
                font = "xft:WenQuanYi Zen Hei:size=10"
              , borderColor = "black"
              , border = TopB
              , bgColor = "black"
              , fgColor = "grey"
              , position = TopW L 100
              -- general behavior
              , lowerOnStart =     False    -- send to bottom of window stack on start
              , hideOnStart =      False   -- start with window unmapped (hidden)
              , allDesktops =      True    -- show on all desktops
              , overrideRedirect = True    -- set the Override Redirect flag (Xlib)
              , pickBroadest =     True   -- choose widest display (multi-monitor)
              , persistent =       True    -- enable/disable hiding (True = disabled)
                    -- <fc=#ee9a00>%date%</fc>
              , template = "%XMonadLog%|%multicpu%|%memory%|%dynnetwork%|%disku%|%diskio%|%coretemp%|%cpufreq%|%WSSS%}{<fc=#ee9a00>%default:Master%|%bright%|%battery%</fc>"
              , sepChar = "%"
              , alignSep = "}{"
              , commands = [ 
                                Run Weather "WSSS" ["-t","<station>: <tempC>C","-L","18","-H","25","--normal","green","--high","red","--low","lightblue"] 36000
                              -- network activity monitor (dynamic interface resolution)
                              , Run DynNetwork     [ "--template" , "<dev>: <tx>kB/s|<rx>kB/s"
                                                    , "--Low"      , "1000"       -- units: kB/s
                                                    , "--High"     , "5000"       -- units: kB/s
                                                    , "--low"      , "green"
                                                    , "--normal"   , "orange"
                                                    , "--high"     , "red"
                                                    ] 10
                              -- cpu activity monitor
                              , Run MultiCpu       [ "--template" , "Cpu: <total0>%|<total1>%"
                                                    , "--Low"      , "50"         -- units: %
                                                    , "--High"     , "85"         -- units: %
                                                    , "--low"      , "green"
                                                    , "--normal"   , "orange"
                                                    , "--high"     , "red"
                                                    ] 10
                              , Run CpuFreq ["-t", "Freq:<cpu0>|<cpu1>GHz", "-L", "0", "-H", "2", "-l", "lightblue", "-n","white", "-h", "red"] 50
                              -- cpu core temperature monitor
                              , Run CoreTemp       [ "--template" , "Temp: <core0>°C|<core1>°C"
                                                    , "--Low"      , "70"        -- units: °C
                                                    , "--High"     , "80"        -- units: °C
                                                    , "--low"      , "green"
                                                    , "--normal"   , "orange"
                                                    , "--high"     , "red"
                                                    ] 50
                              , Run Memory ["-t","Mem: <usedratio>%"] 10
                              , Run Battery [
                                "-t", "<acstatus>: <left>% - <timeleft>",
                                "--",
                                --"-c", "charge_full",
                                "-O", "AC",
                                "-o", "Bat",
                                "-h", "green",
                                "-l", "red"
                                ] 10
                              , Run Com "uname" ["-s","-r"] "" 36000
                              , Run Date "%a %b %_d %Y %H:%M:%S" "date" 10
                             -- mpd
                             -- , Run MPD ["-t","<composer> <title> (<album>) <track>/<plength> <statei> [<flags>]", "--", "-P", ">>", "-Z", "|", "-S", "><"] 10
        	                   -- , Run Com "/bin/bash" ["-c", "~/.script/get-volume.sh"]  "myvolume" 1
                              , Run Volume "default" "Master" [] 10
                              , Run DiskU [("/", "<used>/<size>"), ("sdb1", "<usedbar>")]       ["-L", "20", "-H", "50", "-m", "1", "-p", "3"]   20
                              , Run DiskIO [("/", "<read> <write>"), ("sdb1", "<total>")] [] 10
                              , Run Brightness ["--template", "Bl: <percent>%", "--", "-D", "intel_backlight"] 3
                              , Run XMonadLog
                              ]
              }
      '';
    };
  };

  services = {
    emacs = {
      enable = true;
      defaultEditor = true;
      client = { enable = true; };
      socketActivation = { enable = false; };
    };

    xscreensaver = {
      enable = true;
      settings = {
        mode = "blank";
        lock = false;
        fadeTicks = 20;
      };
    };

    dunst = {
      enable = true;
      iconTheme = {
        name = "Adwaita";
        package = pkgs.gnome.adwaita-icon-theme;
        size = "16x16";
      };
      settings = {
        global = {
          monitor = 0;
          geometry = "600x50-50+65";
          shrink = "yes";
          transparency = 10;
          padding = 16;
          horizontal_padding = 16;
          font = "JetBrainsMono Nerd Font 10";
          line_height = 4;
          format = "<b>%s</b>\\n%b";
          browser = "${pkgs.xdg-utils}/bin/xdg-open";
          dmenu = "${pkgs.rofi}/bin/rofi -dmenu -i -p dunst";
        };
      };
    };

    blueman-applet = { enable = true; };
    dropbox = { enable = true; };
    polybar = {
      enable = true;
      config = {
        colors = {
          background = "#282c34";
          foreground = "#abb2bf";
          tray-background = "#282c34";
        };
        "bar/bottom" = {
          width = "100%";
          height = 40;
          bottom = true;
          background = "\${colors.background}";
          foreground = "\${colors.foreground}";
          modules-left = "date ip pip vpn";
          modules-center = "crypto";
          font-0 = "Hack Nerd Font:size=16";
          tray-position = "right";
          tray-padding = "2";
          tray-background = "\${colors.background}";
          tray-maxsize = 25;
          enable-ipc = true;
        };
        "module/date" = {
          type = "internal/date";
          interval = 5;
          date = "%a %b %d %H:%M";
          format-prefix = " ";
          format-prefix-foreground = "#61afef";
          format-underline = "#61afef";
        };
        "module/crypto" = {
          type = "custom/script";
          exec =
            "${pkgs.curl}/bin/curl https://blockchain.info/ticker --silent | ${pkgs.jq}/bin/jq .USD.last";
          interval = 60;
          format-prefix = " ";
          format-prefix-foreground = "#e06c75";
          format-underline = "#e06c75";
        };
        "module/pip" = {
          type = "custom/script";
          exec = "${pkgs.curl}/bin/curl icanhazip.com --silent";
          interval = 60;
          format-prefix = "  ";
          format-prefix-foreground = "#e06c75";
          format-underline = "#e06c75";
        };
        "module/ip" = {
          type = "custom/script";
          exec =
            "${pkgs.iproute2}/bin/ip a show wlp0s20f3 | ${pkgs.gnugrep}/bin/grep inet | ${pkgs.gawk}/bin/awk '{print $2}' | ${pkgs.coreutils}/bin/head -n 1";
          interval = 60;
          format-prefix = " אַ ";
          format-prefix-foreground = "#e06c75";
          format-underline = "#e06c75";
        };
        "module/vpn" = {
          type = "custom/script";
          exec =
            "${pkgs.iproute2}/bin/ip a show tun0 | ${pkgs.gnugrep}/bin/grep inet | ${pkgs.gawk}/bin/awk '{print $2}' | ${pkgs.coreutils}/bin/head -n 1";
          interval = 60;
          format-prefix = "  ";
          format-prefix-foreground = "#e06c75";
          format-underline = "#e06c75";
        };
      };
      script = "polybar bottom &";
    };
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
      sshKeys = [ "6E215C61D97608ED447E9D8BAE448986D75FD8F6" ];
    };
    picom = {
      enable = true;
      shadow = true;
      fade = true;
      #experimentalBackends= true;
      #backend = "glx";
      shadowExclude = [
        "! name~=''"
        "name = 'Notification'"
        "name = 'Plank'"
        "name = 'Docky'"
        "name = 'Kupfer'"
        "name = 'xfce4-notifyd'"
        "name *= 'VLC'"
        "name *= 'compton'"
        "name *= 'Chromium'"
        "name *= 'Chrome'"
        "name *= 'Firefox'"
        "class_g = 'Conky'"
        "class_g = 'Kupfer'"
        "class_g = 'Synapse'"
        "class_g ?= 'Notify-osd'"
        "class_g ?= 'Cairo-dock'"
        "class_g ?= 'Xfce4-notifyd'"
        "class_g ?= 'Xfce4-power-manager'"
      ];
      shadowOpacity = 0.5;
      vSync = true;

      opacityRules = [
        "85:class_g = 'kitty'"
        "95:class_g = 'Alacritty'"
        "5:class_g = 'emacs'"
        "90:class_g = 'Wine'"
        "90:class_g = 'Thunderbird'"
      ];

      fadeDelta = 2;
      fadeSteps = [ 1.8e-2 1.8e-2 ];

      settings = {
        blur = {
          method = "kawase";
          strength = 5;
          background = false;
          background-frame = false;
          background-fixed = false;
        };
        #inactive-dim = 0.1;
        inactive-opacity = 1;
        inactive-opacity-override = true;
      };

      wintypes = {
        tooltip = {
          # fade: Fade the particular type of windows.
          fade = true;
          # shadow: Give those windows shadow
          shadow = false;
          # opacity: Default opacity for the type of windows.
          opacity = 0.85;
          # focus: Whether to always consider windows of this type focused.
          focus = true;
        };
      };
    };
    udiskie = {
      enable = true;
      automount = true;
      notify = true;
      tray = "always";
    };
    syncthing = {
      enable = true;
      tray = { enable = true; };
    };
  };

}
