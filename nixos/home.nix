{ config, pkgs, lib, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.

  # Packages that should be installed to the user profile.

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  xsession = {
    enable = true;
    initExtra = ''
      ${pkgs.xorg.xset}/bin/xset -b
      ${pkgs.xorg.xset}/bin/xset r rate 180 60
    '';
    windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;
      extraPackages = haskellPackages: [
        haskellPackages.xmonad
        haskellPackages.xmonad-contrib
        haskellPackages.xmonad-extras
      ];
    };
  };
  # gtk = {
  #   enable = true;
  #   iconTheme = {
  #     name = "Adwaita-dark";
  #     package = pkgs.gnome3.adwaita-icon-theme;
  #   };
  #   theme = {
  #     name = "Adwaita-dark";
  #     package = pkgs.gnome3.adwaita-icon-theme;
  #   };
  # };
  i18n = {
    inputMethod = {
      enabled = "fcitx5";
      fcitx5 = { addons = with pkgs; [ fcitx5-chinese-addons ]; };
    };

  };
  home = {
    stateVersion = "22.11";
    file.".wakatime.cfg".source = ../old-files/wakatime/.wakatime.cfg;
    file.".curlrc".text = builtins.readFile ../old-files/downloader/.curlrc;
    file.".editorconfig".text =
      builtins.readFile ../old-files/editor/.editorconfig;
    file.".ssh/id_rsa.pub".text =
      (import ./secret.nix { inherit lib; }).freeman.public-key;
    file.".ssh/id_rsa" = {
      text = (import ./secret.nix { inherit lib; }).freeman.private-key;
      executable = false;
    };
    pointerCursor = {
      name = "Vanilla-DMZ";
      package = pkgs.vanilla-dmz;
      x11 = { enable = true; };
      size = 32;
    };
  };
  programs = {
    alacritty = {
      enable = true;
      settings = { font = { size = 9; }; };

    };
    urxvt = {
      enable = true;
      extraConfig = {
        depth = 32;
        perl-ext-common = "default,matcher,selection-to-clipboard";
        url-launcher = "${pkgs.brave}/bin/brave";
        underlineURLs = "true";
        "matcher.button" = "1";
        "clipboard.autocopy" = true;
        preeditType = "OverTheSpot";
        secondaryScroll = true;
        saveLines = 32767;
        foreground = "#CCCCCC";
        background = "#1B1D1E";
        color0 = "#1B1D1E";
        color8 = "#808080";
        color1 = "#FF0044";
        color9 = "#F92672";
        color2 = "#82B414";
        color10 = "#A6E22E";
        color3 = "#FD971F";
        color11 = "#E6DB74";
        color4 = "#266C98";
        color12 = "#7070F0";
        color5 = "#AC0CB1";
        color13 = "#D63AE1";
        color6 = "#AE81FF";
        color14 = "#66D9EF";
        color7 = "#CCCCCC";
        color15 = "#F8F8F2";
        skipBuiltinGlyphs = false;
      };
      fonts = [ "xft:Hack Nerd Font:size=13" ];
      iso14755 = false;
      keybindings = {
        "Shift-Control-c" = "eval:selection_to_clipboard";
        "Shift-Control-v" = "eval:paste_clipboard";
      };
      shading = 20;
      transparent = true;
      scroll = { bar = { enable = false; }; };
    };
    ssh = {
      enable = true;
      hashKnownHosts = true;
      compression = true;
      matchBlocks = (pkgs.callPackage ./secret.nix { inherit lib; }).hosts;
    };

    bat = { enable = true; };
    exa = { enable = true; };
    command-not-found.enable = true;

    # Let Home Manager install and manage itself.

    home-manager.enable = true;
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
            };

            "HDMI-1" = {
              enable = true;
              crtc = 1;
              primary = false;
              position = "1920x0";
              mode = "3840x2160";
              rate = "60.00";
              rotate = "left";
            };

          };
          # hooks.postswitch = ''
          #   polybar-msg cmd restart
          # '';
        };
      };
    };

    aria2 = {
      enable = true;
      settings = {
        enable-rpc = true;
        #允许所有来源, web界面跨域权限需要
        rpc-allow-origin-all = true;
        #允许外部访问，false的话只监听本地端口
        rpc-listen-all = true;
        #RPC端口, 仅当默认端口被占用时修改
        #rpc-listen-port=6800
        #最大同时下载数(任务数), 路由建议值: 3
        max-concurrent-downloads = 5;
        #断点续传
        continue = true;
        #同服务器连接数
        max-connection-per-server = 5;
        #最小文件分片大小, 下载线程数上限取决于能分出多少片, 对于小文件重要
        min-split-size = "10 M";
        #单文件最大线程数, 路由建议值: 5
        split = 10;
        #下载速度限制
        max-overall-download-limit = 0;
        #单文件速度限制
        max-download-limit = 0;
        #上传速度限制
        max-overall-upload-limit = 0;
        #单文件速度限制
        max-upload-limit = 0;
        #断开速度过慢的连接
        #lowest-speed-limit=0
        #验证用，需要1.16.1之后的release版本
        #referer=*
        #文件保存路径, 默认为当前启动位置
        dir = "/home/chenyu/Downloads";
        #文件缓存, 使用内置的文件缓存, 如果你不相信Linux内核文件缓存和磁盘内置缓存时使用, 需要1.16及以上版本
        #disk-cache=0
        #另一种Linux文件缓存方式, 使用前确保您使用的内核支持此选项, 需要1.15及以上版本(?)
        enable-mmap = true;
        #文件预分配, 能有效降低文件碎片, 提高磁盘性能. 缺点是预分配时间较长
        #所需时间 none < falloc ? trunc << prealloc, falloc和trunc需要文件系统和内核支持
        file-allocation = "prealloc";
      };
    };
    direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv = { enable = true; };
    };

    gh = {
      enable = true;
      settings = { git_protocal = "ssh"; };
    };
    gpg = {
      enable = true;
      settings = {
        keyserver = "hkp://keyserver.ubuntu.com";
        fixed-list-mode = true;
        keyid-format = "0xlong";
        list-options = "show-uid-validity";
        cert-digest-algo = "SHA256";
      };
    };
    git = {
      enable = true;
      lfs = { enable = true; };
      aliases = {
        trash =
          "!mkdir -p .trash && git ls-files --others --exclude-standard | xargs mv -f -t .trash";
      };
      signing = {
        key = "5AF7AFBF695E8A5D";
        signByDefault = true;
      };
      delta = {
        enable = true;
        options = {
          navigate = true;
          line-numbers = true;
          #   syntax-theme = "GitHub";
        };
      };
      extraConfig = {
        push = { default = "current"; };
        color = { ui = "auto"; };
        core = {
          autocrlf = "input";
          editor = "emacs";
        };
        pull = { rebase = false; };
        user = {
          name = "freeman";
          email = "xiongchenyu6@gmail.com";
        };
      };
      ignores = [
        "tags"
        "*.DS_Store"
        "*.sw[nop]"
        ".bundle"
        ".env"
        "db/*.sqlite3"
        "log/*.log"
        "rerun.txt"
        "tmp/**/*"
        "workspace.xml"
        ".idea/"
        "node_modules/"
        "target"
        "!target/native/include/*"
        ".meteor/"
        ".vim/"
        "Debug/"
        "compile_commands.json"
        "tests/CMakeCache.txt"
        ".ccls-cache/*"
        ".ccls-cache/"
        "**/.ensime*"
        ".metals/"
        ".bloop/"
        "dist"
        "dist-*"
        "cabal-dev"
        "*.o"
        "*.hi"
        "*.chi"
        "*.chs.h"
        "*.dyn_o"
        "*.dyn_hi"
        ".hpc"
        ".hsenv"
        ".cabal-sandbox/"
        "cabal.sandbox.config"
        "*.prof"
        "*.aux"
        "*.hp"
        "*.eventlog"
        ".stack-work/"
        "cabal.project.local"
        "cabal.project.local~"
        ".HTF/"
        ".ghc.environment.*"
        "nohup.out"
        ".attach_pid*"
      ];
    };
    zsh = {
      enable = true;
      autocd = true;
      shellAliases = {
        vi = "vim";
        yolo = ''git commit -m "$(curl -s whatthecommit.com/index.txt)"'';
        op = "xdg-open";
        ls = "exa --icons";
      };
      initExtra = ''
        function gre {
           VERSION=$(git describe --abbrev=0 --tags)
           
           #replace . with space so can split into an array
           
           read -r -a VERSION_BITS <<< "''${VERSION//./ }"
           
           #get number parts and increase last one by 1
           VNUM1=''${VERSION_BITS[0]}
           VNUM2=''${VERSION_BITS[1]}
           VNUM3=''${VERSION_BITS[2]}
           VNUM3=$((VNUM3+1))
           
           #create new tag
           NEW_TAG="$VNUM1.$VNUM2.$VNUM3"
           
           echo "Updating $VERSION to $NEW_TAG"
           
           #get current hash and see if it already has a tag
           GIT_COMMIT=$(git rev-parse HEAD)
           NEEDS_TAG=$(git describe --contains "$GIT_COMMIT")
           
           #only tag if no tag already
           if [ -z "$NEEDS_TAG" ]; then
               git tag "$NEW_TAG"
               echo "Tagged with $NEW_TAG"
               git push --tags
           else
               echo "Already a tag on this commit"
           fi
        }
      '';
      oh-my-zsh = {
        enable = true;
        plugins = [
          "aws"
          "cabal"
          "catimg"
          "colored-man-pages"
          "copyfile"
          "docker"
          "docker-compose"
          "extract"
          "encode64"
          "fzf"
          "fancy-ctrl-z"
          "git"
          "git-flow"
          "git-auto-fetch"
          "git-hubflow"
          "github"
          "gitignore"
          "golang"
          "httpie"
          "heroku"
          "jsontools"
          "kubectl"
          "npm"
          "node"
          "pass"
          "pipenv"
          "pip"
          "ripgrep"
          "redis-cli"
          "sbt"
          "scala"
          "systemd"
          "tmux"
        ];
      };
      plugins = [
        {
          name = "alias-tips";
          src = pkgs.fetchFromGitHub {
            owner = "djui";
            repo = "alias-tips";
            rev = "8fc0d2f9b480991f78ce67c49621731d0336b22f";
            sha256 = "sha256-b6b5/m0oinrB2jIPLFohWjZBhDbNJmi537u5pgzaefc=";
          };
        }
        {
          name = "wakatime-zsh-plugin";
          src = pkgs.fetchFromGitHub {
            owner = "sobolevn";
            repo = "wakatime-zsh-plugin";
            rev = "0.1.1";
            sha256 = "sha256-9/JGJgfAjXLIioCo3gtzCXJdcmECy6s59Oj0uVOfuuo=";
          };
        }
        {
          name = "forgit";
          src = pkgs.fetchFromGitHub {
            owner = "wfxr";
            repo = "forgit";
            rev = "3f50933f047510020428114551da0ee5cdfb32a3";
            sha256 = "sha256-TSF4Vr5uf/+MVU4yCdIHNnwB7kkp4mF+hkhKtLqQvmk=";

          };
        }
      ];
      enableCompletion = true;
      enableAutosuggestions = true;
      enableSyntaxHighlighting = true;
    };
    fzf = { enable = true; };
    jq = { enable = true; };
    man = { enable = true; };
    qutebrowser = { enable = true; };
    vim = { enable = true; };
    #  zathura = { enable = true; };
    zoxide = { enable = true; };
    tmux = {
      enable = true;
      terminal = "screen-256color";
      shortcut = "space";
      plugins = with pkgs.tmuxPlugins; [ yank ];
      secureSocket = false;
      keyMode = "vi";

    };
    starship = {
      enable = true;
      settings = {
        format = "$directory$character";
        # move the rest of the prompt to the right
        right_format = "$all";
        # A continuation prompt that displays two filled in arrows
        continuation_prompt = "▶▶";
        kubernetes = {
          disabled = false;

        };
        directory = {
          truncation_length = 20;
          truncation_symbol = "…/";
        };
        status.disabled = false;
        time.disabled = false;
        git_metrics.disabled = false;
        sudo.disabled = false;
      };
    };
    xmobar = {
      enable = true;
      extraConfig = ''
        Config {
                font = "xft:WenQuanYi Zen Hei:size=12"
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

    sbt = {
      enable = true;
      plugins = [
        {
          artifact = "sbt-updates";
          org = "com.timushev.sbt";
          version = "latest.integration";
        }
        {
          artifact = "sbt-stats";
          org = "com.orrsella";
          version = "latest.integration";
        }
      ];
    };
    emacs = {
      enable = true;
      package = pkgs.emacsGitNativeComp;
      extraPackages = epkgs: with epkgs; [
        epkgs.vterm
        epkgs.org-contrib
        epkgs.org-roam
        epkgs.org-re-reveal
        epkgs.pdf-tools
        epkgs.leetcode
      ];
    };
    go = {
      enable = true;
      
    };
  };
  services = {
    emacs = {
      enable = true;
      defaultEditor = true;
      client = {
        enable = true;
      };
    };

    xscreensaver = {
      enable = true;
      # settings = {
      #   mode = "blank";
      #   lock = false;
      #   fadeTicks = 20;
      # };
    };
    dunst = {
      enable = true;
      iconTheme = {
        name = "Adwaita";
        package = pkgs.gnome3.adwaita-icon-theme;
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
        };
      };
    };

    blueman-applet.enable = true;
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
          height = "30";
          bottom = true;
          background = "\${colors.background}";
          foreground = "\${colors.foreground}";
          modules-left = "date ip pip vpn";
          modules-center = "crypto";
          font-0 = "Hack Nerd Font:size=14";
          tray-position = "right";
          tray-padding = "2";
          tray-background = "\${colors.background}";
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
          exec =
            "${pkgs.curl}/bin/curl icanhazip.com --silent";
          interval = 60;
          format-prefix = "  ";
          format-prefix-foreground = "#e06c75";
          format-underline = "#e06c75";
        };
        "module/ip" = {
          type = "custom/script";
          exec =
            "${pkgs.iproute}/bin/ip a show wlp0s20f3 | ${pkgs.gnugrep}/bin/grep inet | ${pkgs.gawk}/bin/awk '{print $2}' | ${pkgs.coreutils}/bin/head -n 1";
          interval = 60;
          format-prefix = " אַ ";
          format-prefix-foreground = "#e06c75";
          format-underline = "#e06c75";
        };
        "module/vpn" = {
          type = "custom/script";
          exec =
            "${pkgs.iproute}/bin/ip a show tun0 | ${pkgs.gnugrep}/bin/grep inet | ${pkgs.gawk}/bin/awk '{print $2}' | ${pkgs.coreutils}/bin/head -n 1";
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
  };
}
