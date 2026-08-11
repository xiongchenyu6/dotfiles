# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running 'nixos-help').
{ config, pkgs, ... }:
{
  home = {
    # osessionVariables = {
    #   SSH_AUTH_SOCK = "$(gpgconf --list-dirs agent-ssh-socket)";
    # };
  };
  programs = {
    zsh = {
      enable = true;
      autocd = true;
      shellAliases = {
        vi = "nvim";
        o = "xdg-open";
        ls = "eza --icons";
        update = "sudo nixos-rebuild switch";
        # View GBK-encoded files (Chinese Windows apps under wine)
        catgbk = "iconv -f GBK -t UTF-8";
        catgb18030 = "iconv -f GB18030 -t UTF-8";
      };
      dirHashes = {
        docs = "$HOME/Documents";
        vids = "$HOME/Videos";
        dl = "$HOME/Downloads";
      };
      dotDir = "${config.xdg.configHome}/zsh";
      envExtra = "";
      zprof.enable = false; # Set to true to enable profiling
      history = {
        extended = true;
        ignoreDups = true;
        ignoreSpace = true;
        share = true;
        size = 50000;
        save = 50000;
        ignorePatterns = [ "rm -rf *" ];
      };
      # HIST_FCNTL_LOCK uses fcntl() locks instead of the default link-based
      # lock — required for safe concurrent writes when SHARE_HISTORY is on.
      # INC_APPEND_HISTORY flushes each command immediately so a killed shell
      # cannot leave a half-written entry behind.
      setOptions = [
        "HIST_FCNTL_LOCK"
        "INC_APPEND_HISTORY"
      ];

      initContent = ''
        # 代理开关(sing-box 本地 mixed 端口);差旅时不 pon 就是全直连
        function pon {
          export http_proxy=http://127.0.0.1:20171 https_proxy=http://127.0.0.1:20171
          export HTTP_PROXY=$http_proxy HTTPS_PROXY=$https_proxy
          export all_proxy=socks5://127.0.0.1:20170 ALL_PROXY=socks5://127.0.0.1:20170
          export no_proxy="localhost,127.0.0.1,::1,.local" NO_PROXY=$no_proxy
          echo "proxy on ($http_proxy)"
        }
        function poff {
          unset http_proxy https_proxy HTTP_PROXY HTTPS_PROXY all_proxy ALL_PROXY no_proxy NO_PROXY
          echo "proxy off"
        }

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
      completionInit = ''
        # Speed up compinit by only checking cached .zcompdump once a day
        autoload -Uz compinit
        for dump in ${config.xdg.configHome}/zsh/.zcompdump(N.mh+24); do
          compinit
        done
        compinit -C
      '';
      enableCompletion = true;
      autosuggestion.enable = true;
      syntaxHighlighting.enable = true;
      # zsh-abbr = {
      #   enable = true;
      #   abbreviations = {
      #     "--global ns" = "nixos-rebuild switch";
      #   };
      # };

      antidote = {
        enable = true;
        plugins = [
          "ohmyzsh/ohmyzsh path:lib"
          # "ohmyzsh/ohmyzsh path:plugins/aliases"
          # "ohmyzsh/ohmyzsh path:plugins/alias-finder"
          "ohmyzsh/ohmyzsh path:plugins/colored-man-pages"
          "ohmyzsh/ohmyzsh path:plugins/copypath"
          "ohmyzsh/ohmyzsh path:plugins/copybuffer"
          # "ohmyzsh/ohmyzsh path:plugins/cp"
          "ohmyzsh/ohmyzsh path:plugins/extract"
          # "ohmyzsh/ohmyzsh path:plugins/encode64"
          "ohmyzsh/ohmyzsh path:plugins/fancy-ctrl-z"
          "ohmyzsh/ohmyzsh path:plugins/git"
          # "ohmyzsh/ohmyzsh path:plugins/gitignore"
          # "ohmyzsh/ohmyzsh path:plugins/rsync"
          "ohmyzsh/ohmyzsh path:plugins/sudo"
          # "ohmyzsh/ohmyzsh path:plugins/systemadmin"
          # "ohmyzsh/ohmyzsh path:plugins/kubectl"
          # "ohmyzsh/ohmyzsh path:plugins/emacs"
        ];
      };
    };
  };
}
