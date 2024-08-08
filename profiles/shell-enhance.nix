# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{ pkgs, ... }:
{
  programs = {
    zsh = {
      enable = true;
      autosuggestions = {
        enable = true;
      };
      syntaxHighlighting = {
        enable = true;
      };
      enableBashCompletion = true;

      shellAliases = {
        vi = "vim";
        yolo = ''git commit -m "$(curl -s whatthecommit.com/index.txt)"'';
        op = "xdg-open";
        ls = "exa --icons";
      };
      ohMyZsh = {
        enable = false;
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
    };
    git = {
      enable = true;
      lfs = {
        enable = true;
      };
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
}
