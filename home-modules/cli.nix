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
        "AB721FF9682FF07B88063C8FADEB89B859C7ACB1"
        "42C87EA7DAAD37765EB1DD0FF53339EFBBF5785C"
      ];
    };
  };

  programs = {

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

    bat = {
      enable = true;
    };

    git-cliff = {
      enable = true;
    };

    # navi = { enable = true; };
    pandoc = {
      enable = true;
    };

    sqls = {
      enable = true;
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
        misc = {
          /*
            disable = [
                     "system"
                     "emacs"
                     "nix"
                     "home_manager"
                     "helm"
                     "bun"
                   ];
          */
          pre_sudo = false;

          only = [
            "system"
            "git_repos"
            "tldr"
            "vscode"
          ];

        };
        linux = {
          # nix_arguments = "--flake";

        };
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
      enable = false;
    };
  };
}
# ocng
