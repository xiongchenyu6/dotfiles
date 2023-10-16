{ pkgs, ... }: {
  editorconfig = {
    enable = true;
    settings = {
      "*" = {
        indent_size = 2;
        end_of_line = "lf";
        insert_final_newline = true;
      };
      "*.{js,py}" = { charset = "utf-8"; };
      "*.css" = { charset = "utf-8"; };
      "*.{py,cpp,c,h,proto}" = {
        indent_style = "space";
        indent_size = 4;
      };

      "Makefile" = { indent_style = "tab"; };
      "lib/**.js" = { indent_style = "space"; };
      "{package.json,.travis.yml}" = { indent_style = "space"; };
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
                favorites = [ "all" "kube-system" "default" ];
              };
              view = { active = "po"; };
              featureGates = { nodeShell = true; };
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
                favorites = [ "all" "kube-system" "default" ];
              };
              view = { active = "po"; };
              featureGates = { nodeShell = true; };
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
                favorites = [ "all" "kube-system" "default" ];
              };
              view = { active = "po"; };
              featureGates = { nodeShell = true; };
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
    readline = { enable = true; };

    atuin = {
      enable = true;
      settings = {
        auto_sync = true;
        sync_frequency = "1m";
        sync_address = "https://atuin.inner.autolife-robotics.tech";
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
        GSSAPIAuthentication yes
        PasswordAuthentication yes
        # PubkeyAcceptedKeyTypes rsa-sha2-256,ssh-ed25519,ecdsa-sha2-nistp256,ecdsa-sha2-nistp384,ecdsa-sha2-nistp521,ssh-rsa
      '';
    };

    bat = { enable = true; };

    eza = { enable = true; };

    home-manager = { enable = true; };

    direnv = {
      enable = true;
      nix-direnv = { enable = true; };
    };

    git-cliff = { enable = true; };

    git = {
      enable = true;
      lfs = { enable = true; };
      aliases = {
        trash =
          "!mkdir -p .trash && git ls-files --others --exclude-standard | xargs mv -f -t .trash";
        pushall = "!git remote | xargs -L1 git push --all";
        rank = "shortlog -s -n --no-merges";
      };

      difftastic = {
        enable = true;
        background = "dark";
      };

      extraConfig = {
        init = { defaultBranch = "main"; };
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

    jq = { enable = true; };

    man = { enable = true; };

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

    zoxide = { enable = true; };

    pandoc = { enable = true; };

    sqls = { enable = true; };

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
        kubernetes = { disabled = false; };
        # directory = {
        #   truncation_length = 20;
        #   truncation_symbol = "…/";
        # };
        status = { disabled = false; };
        time = { disabled = false; };
        git_metrics = { disabled = false; };
        sudo = { disabled = false; };
      };
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
          URI     ldaps://mail.autolife-robotics.tech
          BASE    dc=freeman,dc=engineer
          SASL_MECH GSSAPI
          SASL_REALM autolife-robotics.tech
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
      ".config/electron25-flags.conf" = {
        text = ''
          --enable-features=UseOzonePlatform --ozone-platform=wayland
        '';
      };
      ".config/electron24-flags.conf" = {
        text = ''
          --enable-features=UseOzonePlatform --ozone-platform=wayland
        '';
      };
      ".config/electron23-flags.conf" = {
        text = ''
          --enable-features=UseOzonePlatform --ozone-platform=wayland
        '';
      };
      ".config/electron22-flags.conf" = {
        text = ''
          --enable-features=UseOzonePlatform --ozone-platform=wayland
        '';
      };
      ".config/code-flags.conf" = {
        text = ''
          --enable-features=UseOzonePlatform --ozone-platform=wayland
        '';
      };

    };
    stateVersion = "23.11";
    keyboard = { options = [ "caps:ctrl_modifier" ]; };
  };
}
