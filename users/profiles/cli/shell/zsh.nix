# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ pkgs, ... }: {
  programs = {
    zsh = let
      ohMyZsh2Zplug = builtins.map (p: {
        name = "plugins/${p}";
        tags = [ "from:oh-my-zsh" ];
      });
      plugins = [
        "catimg"
        "colored-man-pages"
        "copyfile"
        "copypath"
        "docker"
        "emacs"
        "extract"
        "encode64"
        "fancy-ctrl-z"
        "git"
        "git-flow"
        "gitignore"
        "helm"
        "kubectl"
        "otp"
        "pass"
        "redis-cli"
        "ripgrep"
        "rsync"
        "sudo"
        "systemd"
        "systemadmin"
        "scala"
        "terraform"
        "tmux"
      ];

    in {
      enable = true;
      autocd = true;
      shellAliases = {
        vi = "vim";
        o = "xdg-open";
        ls = "exa --icons";
        update = "sudo nixos-rebuild switch";
      };
      dirHashes = {
        docs = "$HOME/Documents";
        vids = "$HOME/Videos";
        dl = "$HOME/Downloads";
      };
      dotDir = ".config/zsh";
      envExtra = "";
      history = {
        extended = true;
        ignorePatterns = [ "rm -rf *" ];
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
        path+="$HOME/.npm/bin"
        eval $(${pkgs.bash-my-aws}/bin/bma-init)
        eval $(${pkgs.rustup}/bin/rustup completions zsh)
        complete -C '${pkgs.awscli2}/bin/aws_completer' aws
      '';
      zplug = {
        enable = false;
        plugins = ohMyZsh2Zplug plugins;
      };
      oh-my-zsh = {
        enable = true;
        inherit plugins;
      };
      plugins = let
        source = with pkgs;
          callPackage ./_sources/generated.nix {
            inherit fetchFromGitHub fetchurl fetchgit;
          };
      in map (name:
        (removeAttrs source.${name} [ "pname" "version" "date" ]) // {
          name = "${name}";
        }) [ "alias-tips" "wakatime-zsh-plugin" ];
      enableCompletion = true;
      enableAutosuggestions = true;
      enableSyntaxHighlighting = true;
    };
  };
}
