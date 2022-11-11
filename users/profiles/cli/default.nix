{
  pkgs,
  lib,
  profiles,
  ...
}: {
  # Home Manager needs a bit of information about you and the
  # paths it should manage.

  home = {
    stateVersion = "22.11";
    keyboard = {options = ["caps:ctrl_modifier"];};
    file = let
      old-files-path = ../../../old-files;
    in {
      ".wakatime.cfg" = {source = old-files-path + /wakatime/.wakatime.cfg;};
      ".ldaprc" = {source = old-files-path + /ldap/.ldaprc;};
      ".curlrc" = {source = old-files-path + /downloader/.curlrc;};
      ".ssh/id_ed25519.pub" = {
        text = profiles.share.office.user.public-key;
        executable = false;
      };
      ".ssh/id_ed25519" = {
        source = ../../../secrets/office_pk.key;
        executable = false;
      };
    };
  };

  # Packages that should be installed to the user profile.

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.

  editorconfig = {
    enable = true;
    settings = {
      "*" = {
        end_of_line = "lf";
        insert_final_newline = true;
      };
      "*.{js,py}" = {charset = "utf-8";};
      "*.{py,cpp,c,h,proto}" = {
        indent_style = "space";
        indent_size = 4;
      };

      "Makefile" = {indent_style = "tab";};
      "lib/**.js" = {
        indent_style = "space";
        indent_size = 2;
      };
      "{package.json,.travis.yml}" = {
        indent_style = "space";
        indent_size = 2;
      };
    };
  };

  programs = {
    btop = {enable = true;};

    # keychain = {
    #   enable = true;
    # };

    readline = {enable = true;};
    # nushell = { enable = true; };

    atuin = {
      enable = true;
      settings = {
        auto_sync = true;
        sync_frequency = "1m";
        sync_address = "https://api.atuin.sh";
      };
    };

    ssh = {
      enable = true;
      hashKnownHosts = true;
      compression = true;
      # controlMaster = "auto";
      matchBlocks = {
        "freeman.engineer" = {port = 2222;};
        "mail.freeman.engineer" = {
          port = 2222;
          user = "root";
        };
        "git-code-commit.*.amazonaws.com" = lib.hm.dag.entryBefore ["freeman.engineer"] {
          user = "APKA6ECL465SUMKZQKLN";
        };
      };
      extraConfig = ''
        GSSAPIAuthentication yes
        PasswordAuthentication yes
      '';
    };

    bat = {enable = true;};
    exa = {enable = true;};

    # Let Home Manager install and manage itself.

    home-manager = {enable = true;};

    direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv = {enable = true;};
    };

    git = {
      enable = true;
      lfs = {enable = true;};
      aliases = {
        trash = "!mkdir -p .trash && git ls-files --others --exclude-standard | xargs mv -f -t .trash";
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
        push = {default = "current";};
        color = {ui = "auto";};
        core = {
          autocrlf = "input";
          editor = "emacs";
        };
        pull = {rebase = false;};
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
    zsh = {
      enable = true;
      autocd = true;
      shellAliases = {
        vi = "vim";
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
        export LSP_USE_PLISTS=true
        eval $(${pkgs.bash-my-aws}/bin/bma-init)
      '';
      oh-my-zsh = {
        enable = true;
        plugins = [
          "catimg"
          "colored-man-pages"
          "copyfile"
          "copypath"
          "emacs"
          "extract"
          "encode64"
          "fancy-ctrl-z"
          "git"
          "git-flow"
          "git-hubflow"
          "gitignore"
          "pass"
          "ripgrep"
          "rsync"
          "sudo"
          "systemd"
          "scala"
          "tmux"
        ];
      };
      plugins = let
        source = with pkgs;
          callPackage ./_sources/generated.nix {
            inherit fetchFromGitHub fetchurl fetchgit;
          };
      in
        map (name:
          (removeAttrs source.${name} ["pname" "version" "date"])
          // {
            name = "${name}";
          }) ["alias-tips" "wakatime-zsh-plugin"];
      enableCompletion = false;
      enableAutosuggestions = true;
      enableSyntaxHighlighting = true;
    };

    jq = {enable = true;};

    man = {enable = true;};

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
        set clipboard+=unnamed  " use the clipboards of vim and win
        set paste               " Paste from a windows or from vim
        set go+=a               " Visual selection automatically copied to the clipboard
      '';
    };

    zoxide = {enable = true;};

    tmux = {
      enable = true;
      terminal = "screen-256color";
      shortcut = "space";
      plugins = with pkgs.tmuxPlugins; [yank];
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
        kubernetes = {disabled = false;};
        directory = {
          truncation_length = 20;
          truncation_symbol = "…/";
        };
        status = {disabled = false;};
        time = {disabled = false;};
        git_metrics = {disabled = false;};
        sudo = {disabled = false;};
      };
    };
  };
}
