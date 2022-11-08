# Edit this configuration file to define what should be installed on

# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  system = { stateVersion = "22.11"; }; # Did you read the comment?

  system.nixos = { tags = [ "test" "add-feat" ]; };

  programs = {
    zsh = {
      enable = true;
      shellAliases = {
        vi = "vim";
        yolo = ''git commit -m "$(curl -s whatthecommit.com/index.txt)"'';
        op = "xdg-open";
        ls = "exa --icons";
      };
      ohMyZsh = {
        enable = true;
        plugins = [
          "aws"
          "cabal"
          "catimg"
          "colored-man-pages"
          "colorize"
          "command-not-found"
          "copyfile"
          "docker"
          "docker-compose"
          "direnv"
          "extract"
          "encode64"
          "emacs"
          "fancy-ctrl-z"
          "git"
          "git-flow"
          "git-auto-fetch"
          "git-hubflow"
          "github"
          "gitignore"
          "gpg-agent"
          "golang"
          "httpie"
          "heroku"
          "jsontools"
          "kubectl"
          "npm"
          "node"
          "pass"
          "pipenv"
          "pip"
          "ripgrep"
          "redis-cli"
          "sbt"
          "scala"
          "systemd"
          "tmux"
        ];
      };
      setOptions = [
        "BANG_HIST"
        "EXTENDED_HISTORY"

        "INC_APPEND_HISTORY"
        "SHARE_HISTORY"
        "HIST_EXPIRE_DUPS_FIRST"
        "HIST_IGNORE_DUPS"
        "HIST_IGNORE_ALL_DUPS"
        "HIST_FIND_NO_DUPS"
        "HIST_IGNORE_SPACE"
        "HIST_SAVE_NO_DUPS"
        "HIST_REDUCE_BLANKS"
        "HIST_VERIFY"
        "HIST_BEEP"

      ];
      enableCompletion = true;
      autosuggestions = { enable = true; };
      syntaxHighlighting = { enable = true; };
      enableBashCompletion = true;
    };
    git = {
      enable = true;
      lfs = { enable = true; };
    };
    tmux = {
      enable = true;
      terminal = "screen-256color";
      shortcut = "space";
      plugins = with pkgs.tmuxPlugins; [ yank ];
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
        kubernetes = {
          disabled = false;

        };
        directory = {
          truncation_length = 20;
          truncation_symbol = "…/";
        };
        status.disabled = false;
        time.disabled = false;
        git_metrics.disabled = false;
        sudo.disabled = false;
      };
    };
  };

  nixpkgs = {
    config = {
      allowUnfree = true;
      allowBroken = true;
      allowUnsupportedSystem = true;
    };
  };

  nix = {
    daemonCPUSchedPolicy = "idle";
    daemonIOSchedClass = "idle";
    gc = {
      automatic = true;
      options = "--delete-older-than 7d";
      randomizedDelaySec = "1h";
    };
    optimise.automatic = true;
    settings = {
      allow-import-from-derivation = true;
      experimental-features =
        [ "nix-command" "flakes" "repl-flake" "ca-derivations" ];
      trusted-users = [ "root" "freeman" ];

      auto-optimise-store = true;
      substituters = [
        "https://nix-community.cachix.org"
        "https://xddxdd.cachix.org"
        "https://colmena.cachix.org"
        "https://cache.nixos.org"
        "https://xiongchenyu6.cachix.org"
      ];
      trusted-public-keys = [
        "xddxdd.cachix.org-1:ay1HJyNDYmlSwj5NXQG065C8LfoqqKaTNCyzeixGjf8="
        "colmena.cachix.org-1:7BzpDnjjH8ki2CT3f6GdOk7QAzPOl+1t3LvTLXqYcSg="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "xiongchenyu6.cachix.org-1:mpOGlINmMwc2gb3xb1BjVmhzR8BYWzWYlg4xlTiBr7Q="
      ];
    };
  };

  nix.distributedBuilds = false;
  nix.buildMachines = [{
    hostName = "hydra.inner.trontech.link";
    sshUser = "michael.yang";
    systems = [ "x86_64-linux" ];
    maxJobs = 2;
  }];

}
