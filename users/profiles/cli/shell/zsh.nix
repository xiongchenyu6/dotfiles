# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{pkgs, ...}: {
  programs = {
    zsh = {
      enable = true;
      autocd = true;
      shellAliases = {
        vi = "vim";
        o = "handlr open";
        ls = "exa --icons";
      };
      dirHashes = {
        docs = "$HOME/Documents";
        vids = "$HOME/Videos";
        dl = "$HOME/Downloads";
      };
      dotDir = ".config/zsh";
      envExtra = ''
        export LSP_USE_PLISTS=true
      '';
      history = {
        extended = true;
        ignorePatterns = ["rm -rf *"];
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
        eval $(${pkgs.bash-my-aws}/bin/bma-init)
        path+="$HOME/.npm/bin"
        # complete -C '${pkgs.awscli2}/bin/aws_completer' aws

        # function restricted-expand-or-complete() {
        #         if [[ ! $PWD = /mnt/* ]]; then
        #                 zle expand-or-complete
        #         else
        #                 echo -en "\007"
        #         fi
        # }
        # zle -N restricted-expand-or-complete
        # bindkey "^I" restricted-expand-or-complete
      '';
      zplug = let
        ohMyZsh2Zplug = builtins.map (p: {
          name = "plugins/${p}";
          tags = ["from:oh-my-zsh"];
        });
      in {
        enable = false;
        plugins = ohMyZsh2Zplug [
          "catimg"
          "colored-man-pages"
          "copyfile"
          "copypath"
          "emacs"
          "extract"
          "encode64"
          "fancy-ctrl-z"
          "git"
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
  };
}
