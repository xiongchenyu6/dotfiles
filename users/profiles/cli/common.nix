{ config, pkgs, ... }:
{

  xdg = {
    enable = true;
    mime = {
      enable = true;
    };
    mimeApps = {
      enable = true;
    };
    userDirs = {
      enable = true;
      createDirectories = true;
      extraConfig = {
        XDG_MISC_DIR = "${config.home.homeDirectory}/Misc";
        XDG_TEST_DIR = "${config.home.homeDirectory}/Test";
        XDG_GIT_DIR = "${config.home.homeDirectory}/Git";
        XDG_PRIVATE_DIR = "${config.home.homeDirectory}/Private";
        XDG_WORKSPACE_DIR = "${config.home.homeDirectory}/Workspace";
      };
    };
  };

  editorconfig = {
    enable = true;
    settings = {
      "*" = {
        indent_size = 2;
        end_of_line = "lf";
        insert_final_newline = true;
      };
      "*.{js,py}" = {
        charset = "utf-8";
      };
      "*.css" = {
        charset = "utf-8";
      };
      "*.{py,cpp,c,h,proto}" = {
        indent_style = "space";
        indent_size = 4;
      };

      "Makefile" = {
        indent_style = "tab";
      };
      "lib/**.js" = {
        indent_style = "space";
      };
      "{package.json,.travis.yml}" = {
        indent_style = "space";
      };
    };
  };

  services = {
    gpg-agent = {
      enable = true;
      # enableExtraSocket = true;
      # extraConfig = ''
      #   allow-emacs-pinentry
      #   allow-loopback-pinentry
      # '';
      pinentryPackage = pkgs.pinentry-tty;
      enableSshSupport = true;
      # gpg2 -K --with-keygrip 
      sshKeys = [
        "7F799AE1ECC9E828896A5925E8CF69D45DC71164"
        "42C87EA7DAAD37765EB1DD0FF53339EFBBF5785C"
      ];
    };
  };

  programs = {
    btop = {
      enable = true;
      settings = {
        graph_symbol = "braille";
        theme_background = "True";
        show_battery = "True";
        selected_battery = "Auto";
      };
    };

    k9s = {
      enable = true;
      settings = {
        k9s = {
          refreshRate = 2;
          maxConnRetry = 5;
          enableMouse = true;
          headless = false;
          crumbsless = false;
          readOnly = false;
          noIcons = false;
          logger = {
            tail = 200;
            buffer = 500;
            sinceSeconds = 300;
            fullScreenLogs = false;
            textWrap = true;
            showTime = false;
          };
          clusters = {
            dev = {
              namespace = {
                active = "all";
                favorites = [
                  "all"
                  "kube-system"
                  "default"
                ];
              };
              view = {
                active = "po";
              };
              featureGates = {
                nodeShell = true;
              };
              shellPod = {
                namespace = "default";
                limits = {
                  cpu = "100m";
                  memory = "100Mi";
                };
              };
            };
            office = {
              namespace = {
                active = "all";
                favorites = [
                  "all"
                  "kube-system"
                  "default"
                ];
              };
              view = {
                active = "po";
              };
              featureGates = {
                nodeShell = true;
              };
              shellPod = {
                namespace = "default";
                limits = {
                  cpu = "100m";
                  memory = "100Mi";
                };
              };
            };
            prod = {
              namespace = {
                active = "all";
                favorites = [
                  "all"
                  "kube-system"
                  "default"
                ];
              };
              view = {
                active = "po";
              };
              featureGates = {
                nodeShell = true;
              };
              shellPod = {
                namespace = "default";
                limits = {
                  cpu = "100m";
                  memory = "100Mi";
                };
              };
            };
          };
        };
      };
    };

    # keychain = { enable = true; };
    readline = {
      enable = true;
    };

    atuin = {
      enable = true;
      settings = {
        auto_sync = true;
        sync_frequency = "1m";
        sync = {
          records = true;
        };
        #sync_address = "https://atuin.inner.autolife-robotics.me";
      };
    };

    ssh = {
      enable = true;
      hashKnownHosts = false;
      controlMaster = "auto";
      controlPersist = "24h";
      compression = true;
      # tpm chips limitation
      extraConfig = ''
        PasswordAuthentication yes
        # PubkeyAcceptedKeyTypes rsa-sha2-256,ssh-ed25519,ecdsa-sha2-nistp256,ecdsa-sha2-nistp384,ecdsa-sha2-nistp521,ssh-rsa
      '';
    };

    bat = {
      enable = true;
    };

    home-manager = {
      enable = true;
    };

    direnv = {
      enable = true;
      nix-direnv = {
        enable = true;
      };
    };

    git-cliff = {
      enable = true;
    };

    git = {
      enable = true;
      lfs = {
        enable = true;
      };
      aliases = {
        trash = "!mkdir -p .trash && git ls-files --others --exclude-standard | xargs mv -f -t .trash";
        pushall = "!git remote | xargs -L1 git push --all";
        rank = "shortlog -s -n --no-merges";
      };

      difftastic = {
        enable = true;
        background = "dark";
      };

      extraConfig = {
        init = {
          defaultBranch = "main";
        };
        # [url "ssh://git@github.com/"]
        # 	insteadOf = https://github.com/
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
        ".attach_bid*"
      ];
    };

    jq = {
      enable = true;
    };

    man = {
      enable = true;
    };
    gpg = {
      enable = true;
      settings = {
        #keyserver = "hkps://keyserver.ubuntu.com";
        fixed-list-mode = true;
        keyid-format = "0xlong";
        # cert-digest-algo = "SHA256";
        # personal-digest-preferences = "SHA256";
        no-comments = true;
        no-emit-version = true;
        no-greeting = true;
        list-options = "show-uid-validity";
        verify-options = "show-uid-validity";
        with-fingerprint = true;
        require-cross-certification = true;
        no-symkey-cache = true;
        use-agent = true;
        throw-keyids = true;
      };
    };

    vim = {
      enable = true;
      plugins = with pkgs.vimPlugins; [
        ale
        denite
        lightline-vim
        nerdtree
        tagbar
      ];
      settings = {
        expandtab = true;
        history = 1000;
        background = "dark";
      };

      extraConfig = ''
        set clipboard=unnamed,unnamedplus  " use the clipboards of vim and win
        set paste               " Paste from a windows or from vim
        set go+=a               " Visual selection automatically copied to the clipboard
      '';
    };

    # navi = { enable = true; };

    zoxide = {
      enable = true;
    };

    pandoc = {
      enable = true;
    };

    sqls = {
      enable = true;
    };

    tmux = {
      enable = true;
      terminal = "screen-256color";
      shortcut = "space";
      plugins = with pkgs.tmuxPlugins; [ yank ];
      secureSocket = false;
      keyMode = "vi";
      customPaneNavigationAndResize = true;
    };

    starship = {
      enable = true;
      settings = {
        # move the rest of the prompt to the right
        # format = "$character";
        # right_format = "$all";
        # A continuation prompt that displays two filled in arrows
        continuation_prompt = "▶▶";
        kubernetes = {
          disabled = false;
        };
        # directory = {
        #   truncation_length = 20;
        #   truncation_symbol = "…/";
        # };
        status = {
          disabled = false;
        };
        time = {
          disabled = false;
        };
        git_metrics = {
          disabled = false;
        };
        sudo = {
          disabled = false;
        };
      };
    };
    broot = {
      enable = true;
      settings = { };
    };

    nix-index = {
      enable = true;
    };

    java = {
      enable = true;
    };

    topgrade = {
      enable = true;
      settings = {
        assume_yes = true;
        disable = [ "emacs" ];
        set_title = false;
        cleanup = true;
        git = {
          max_concurrency = 10;
          repos = [
            "~/workspace/*/"
            "~/git/*/"
            "~/private/*/"
          ];
          arguments = "--rebase --autostash";
        };
        commands = {
          "emacs straight" = "emacs --batch -l ~/.config/emacs/early-init.el -f straight-pull-all";
        };
      };
    };

    yt-dlp = {
      enable = true;
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
        dir = "/home/freeman.xiong/Downloads";
        #文件缓存, 使用内置的文件缓存, 如果你不相信Linux内核文件缓存和磁盘内置缓存时使用, 需要1.16及以上版本
        #disk-cache=0
        #另一种Linux文件缓存方式, 使用前确保您使用的内核支持此选项, 需要1.15及以上版本(?)
        enable-mmap = true;
        #文件预分配, 能有效降低文件碎片, 提高磁盘性能. 缺点是预分配时间较长
        #所需时间 none < falloc ? trunc << prealloc, falloc和trunc需要文件系统和内核支持
        file-allocation = "prealloc";
      };
    };
    eza = {
      enable = true;
    };
    gh = {
      enable = true;
      settings = {
        git_protocal = "ssh";
      };
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
    go = {
      enable = true;
    };
    yazi = {
      enable = true;
    };

  };
  home = {
    file = {
      ".wakatime.cfg" = {
        text = ''
          [settings]
          debug         = false
          hidefilenames = false
          ignore        = """
              COMMIT_EDITMSG$
              PULLREQ_EDITMSG$
              MERGE_MSG$
              TAG_EDITMSG$"""
          api_key       = 06fb08d0-68a4-4b39-bbb0-d34d325dc046

          [internal]
          backoff_retries = 0
          backoff_at      = 
        '';
      };
      ".ldaprc" = {
        text = ''
          URI     ldap://mail.autolife-robotics.me
          BASE    dc=autolife-robotics,dc=tech
          SASL_MECH GSSAPI
          SASL_REALM AUTOLIFE.TECH
        '';
      };
      ".curlrc" = {
        text = ''
          connect-timeout = 30
          referer = ";auto"
          show-error
          progress-bar
          user-agent = "Mozilla/5.0 Gecko"
        '';
      };
      ".clojure/deps.edn" = {
        text = ''
          {
              :aliases
                 {:new {:extra-deps {seancorfield/clj-new
                                     {:mvn/version "LATEST"}}
                        :main-opts ["-m" "clj-new.create"]}}
        '';
      };
      ".clojure/tools/tools.edn" = {
        text = ''
          {:lib io.github.clojure/tools.tools
           :coord {:git/tag "v0.2.5"
                   :git/sha "76f728dce63f7b881f4e5705ba0d59d795d56f11"}}
        '';
      };
      ".my.conf" = {
        text = ''
          [mysql]
          auto-rehash
          general_log_file        = /var/log/mysql/mysql.log
          general_log             = 1
        '';
      };
      ".aspell" = {
        text = ''
          lang en_US
        '';
      };
      ".config/electron-flags.conf" = {
        text = ''
          --enable-wayland-ime
          --enable-features=WaylandWindowDecorations
          --ozone-platform-hint=auto        '';
      };

      ".config/electron25-flags.conf" = {
        text = ''
          --enable-wayland-ime
          --enable-features=WaylandWindowDecorations
          --ozone-platform-hint=auto        '';
      };
      ".config/electron24-flags.conf" = {
        text = ''
          --enable-wayland-ime
          --enable-features=UseOzonePlatform
          --ozone-platform=wayland 
        '';
      };
      ".config/electron23-flags.conf" = {
        text = ''
          --enable-wayland-ime
          --enable-features=UseOzonePlatform
          --ozone-platform=wayland 
        '';
      };
      ".config/electron22-flags.conf" = {
        text = ''
          --enable-wayland-ime
          --enable-features=UseOzonePlatform
          --ozone-platform=wayland 
        '';
      };
      ".config/code-flags.conf" = {
        text = ''
          --enable-wayland-ime
          --enable-features=WaylandWindowDecorations
          --ozone-platform-hint=auto
        '';
      };
    };
    stateVersion = "24.05";
    keyboard = {
      options = [ "caps:ctrl_modifier" ];
    };
  };
}
# ocng
