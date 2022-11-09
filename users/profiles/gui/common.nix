{ config, pkgs, lib, ... }: {
  # Home Manager needs a bit of information about you and the
  # paths it should manage.

  programs = {
    broot = { enable = true; };

    nix-index = { enable = true; };

    noti = { enable = true; };

    sqls = { enable = true; };

    octant = { enable = true; };

    texlive = { enable = true; };

    alacritty = {
      enable = true;
      settings = {
        font = { size = 12; };
        key_bindings = [{
          key = "Space";
          mods = "Control";
          mode = "~Search";
          action = "ToggleViMode";
        }];
      };
    };

    java = { enable = true; };

    topgrade = {
      enable = true;
      settings = {
        assume_yes = true;
        disable = [ "emacs" ];
        set_title = false;
        cleanup = true;
        git = {
          max_concurrency = 10;
          repos = [ "~/workspace/*/" "~/git/*/" "~/private/*/" ];
          arguments = "--rebase --autostash";
        };
      };
    };

    yt-dlp = { enable = true; };

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

    gh = {
      enable = true;
      settings = { git_protocal = "ssh"; };
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
      extraPackages = epkgs:
        with pkgs;
        with epkgs; [
          ace-link
          all-the-icons
          clojure-mode
          cider
          cmake-mode
          corfu
          corfu-doc
          copilot-el
          dockerfile-mode
          direnv
          eglot
          emmet-mode
          format-all
          dap-mode
          doom-themes
          doom-modeline
          graphviz-dot-mode
          gnu-apl-mode
          gcmh
          go-mode
          haskell-mode
          leetcode
          lispy
          ligature
          lsp-java
          nix-mode
          meow
          magit
          marginalia
          nov
          ox-gfm
          ox-hugo
          ob-mermaid
          ob-restclient
          org-contrib
          org-roam
          org-re-reveal
          org-download
          org-cv
          orderless
          plantuml-mode
          pdf-tools
          posframe
          protobuf-mode
          restclient
          rainbow-delimiters
          racket-mode
          rg
          rust-mode
          solidity-mode
          sml-mode
          scala-mode
          sbt-mode
          tide
          vterm
          wakatime-mode
          which-key
          yasnippet
          yaml-mode
        ];
      extraConfig = ''
        (add-to-list 'default-frame-alist
                     '(font . "JetBrains Mono-14"))
        (setq gnus-init-file "~/.config/emacs/gnus.el")
        (setq custom-file "~/.config/emacs/custom.el")
        (setq yas-snippet-dirs
              '("~/.config/emacs/snippets"))
      '';
    };
    go = { enable = true; };
  };
}
