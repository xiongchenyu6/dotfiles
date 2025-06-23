# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running 'nixos-help').
{ pkgs, ... }:
{
  home = {
    # sessionVariables = {
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
        c = "code --enable-wayland-ime=true";
      };
      dirHashes = {
        docs = "$HOME/Documents";
        vids = "$HOME/Videos";
        dl = "$HOME/Downloads";
      };
      dotDir = ".config/zsh";
      envExtra = "";
      zprof.enable = false; # Set to true to enable profiling
      # history = {
      #   extended = true;
      #   ignorePatterns = [ "rm -rf *" ];
      # };

      initContent = ''
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
           elsepp
               echo "Already a tag on this commit"
           fi
        }

      '';
      completionInit = ''
        # Speed up compinit by only checking cached .zcompdump once a day
        autoload -Uz compinit
        for dump in ~/.config/zsh/.zcompdump(N.mh+24); do
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
