# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running 'nixos-help').
{ pkgs, ... }:
{
  programs = {
    zsh = {
      initContent = ''
        
        eval "$(${pkgs.rustup}/bin/rustup completions zsh)"
        eval "$(${pkgs.grafana-loki}/bin/logcli --completion-script-zsh)"
      '';
      #       complete -C '${pkgs.awscli2}/bin/aws_completer' aws
      # eval $(${pkgs.bash-my-aws}/bin/bma-init)
      antidote = {
        plugins = [

          # "ohmyzsh/ohmyzsh path:plugins/aliases"
          # "ohmyzsh/ohmyzsh path:plugins/alias-finder"
          # "ohmyzsh/ohmyzsh path:plugins/cp"
          # "ohmyzsh/ohmyzsh path:plugins/encode64"
          # "ohmyzsh/ohmyzsh path:plugins/gitignore"
          # "ohmyzsh/ohmyzsh path:plugins/rsync"
          # "ohmyzsh/ohmyzsh path:plugins/systemadmin"
          # "ohmyzsh/ohmyzsh path:plugins/kubectl"
          "ohmyzsh/ohmyzsh path:plugins/emacs"
        ];
      };
    };
  };
}
