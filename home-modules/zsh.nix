# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running 'nixos-help').
{ pkgs, ... }:
{
  programs = {
    zsh = {
      initContent = ''
        complete -C '${pkgs.awscli2}/bin/aws_completer' aws
        eval $(${pkgs.bash-my-aws}/bin/bma-init)
        eval "$(${pkgs.rustup}/bin/rustup completions zsh)"
        eval "$(${pkgs.grafana-loki}/bin/logcli --completion-script-zsh)
        eval "$(${pkgs.gh}/bin/gh copilot alias -- zsh)"
      '';
    };
  };
}
