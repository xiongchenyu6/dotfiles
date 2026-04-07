# Base CLI tools for all users (root + non-root)
{
  pkgs,
  config,
  osConfig,
  lib,
  ...
}:
let
  isRoot = config.home.username == "root";
  hasNixOSTags = osConfig ? system && osConfig.system ? nixos && osConfig.system.nixos ? tags;
  hostName =
    if osConfig ? networking && osConfig.networking ? hostName then
      osConfig.networking.hostName
    else
      "";
  isVps =
    hasNixOSTags
    && builtins.elem hostName [
      "oracle-arm-001"
      "oracle-arm-002"
      "oracle-amd-001"
      "oracle-amd-002"
      "tcloud"
      "netbird"
      "huoshan-bj-001"
    ];
in
{
  imports = [
    ./neovim.nix
  ];

  sops.secrets."wakatime/api_key" = lib.mkIf (!isRoot && !isVps) { };

  sops.templates."wakatime-cfg" = lib.mkIf (!isRoot && !isVps) {
    content = ''
      [settings]
      debug         = false
      hidefilenames = false
      ignore        = """
          COMMIT_EDITMSG$
          PULLREQ_EDITMSG$
          MERGE_MSG$
          TAG_EDITMSG$"""
      api_key       = ${config.sops.placeholder."wakatime/api_key"}

      [internal]
      backoff_retries = 0
      backoff_at      = 
    '';
    path = "${config.home.homeDirectory}/.wakatime.cfg";
    mode = "600";
  };

  modules.neovim.enable = true;
  modules.neovim.lightweight = lib.mkDefault true;

  programs = {
    ripgrep-all = {
      enable = true;
    };
    ripgrep = {
      enable = true;
    };
    pgcli = {
      enable = true;
    };
    fd = {
      enable = true;
    };
    fastfetch = {
      enable = true;
    };
    btop = {
      enable = true;
      settings = {
        graph_symbol = "braille";
        theme_background = "True";
        show_battery = "True";
        selected_battery = "Auto";
      };
    };

    atuin = {
      enable = true;
      settings = {
        auto_sync = true;
        sync_frequency = "1m";
        sync = {
          records = true;
        };
        #sync_address = "https://atuin.inner.${config.networking.domain}";
      };
    };

    ssh = {
      enable = true;
      enableDefaultConfig = false; # Explicitly disable defaults as recommended
      matchBlocks = {
        "*" = {
          # Move all the global options here
          hashKnownHosts = false;
          controlMaster = "auto";
          controlPersist = "24h";
          compression = true;
          forwardAgent = true;
          addKeysToAgent = "yes";
          # forwardX11 = true;
          # forwardX11Trusted = true;
        };
      };
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

    git = {
      enable = true;

      lfs = {
        enable = true;
      };

      signing.format = "openpgp";

      # Updated to new format: settings replaces extraConfig and aliases
      settings = {
        alias = {
          trash = "!mkdir -p .trash && git ls-files --others --exclude-standard | xargs mv -f -t .trash";
          pushall = "!git remote | xargs -L1 git push --all";
          rank = "shortlog -s -n --no-merges";
        };
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
      scdaemonSettings = {
        debug-level = "guru";
        log-file = "${config.home.homeDirectory}/.cache/gnupg/scdaemon.log";
        disable-ccid = true;
      };
      settings = {
        #keyserver = "hkps://keyserver.ubuntu.com";
        fixed-list-mode = true;
        # cert-digest-algo = "SHA256";
        # personal-digest-preferences = "SHA256";
        no-comments = true;
        no-emit-version = true;
        no-greeting = true;
        list-options = "show-uid-validity";
        verify-options = "show-uid-validity";
        with-fingerprint = false;
        require-cross-certification = true;
        no-symkey-cache = true;
        use-agent = true;
        #throw-keyids = true;
      };
    };

    # navi = { enable = true; };

    zoxide = {
      enable = true;
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
          # Only show when command failed (hide success/OK messages)
          format = "[$symbol$status]($style) ";
          success_symbol = "";
          # Only display status on error
          recognize_signal_code = true;
          map_symbol = true;
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

    eza = {
      enable = true;
    };

    # Moved difftastic to its own program block as per the new format
    difftastic = {
      enable = true;
      options = {
        background = "dark";
      };
      git = {
        enable = true; # Explicitly enable git integration as recommended
      };
    };

  };

  # Disable automatic gpg-agent setup to prevent "OK" message on shell startup
  services.gpg-agent.enable = true;

  home = {
    file = {
      ".ldaprc" =
        lib.mkIf
          (osConfig ? networking && osConfig.networking ? domain && osConfig.networking.domain != null)
          {
            text = ''
              URI     ldap://mail.${osConfig.networking.domain}
              BASE    dc=autolife,dc=ai
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
    };
    stateVersion = "26.05";
    keyboard = {
      options = [ "caps:ctrl_modifier" ];
    };
  };
}
# ocng
