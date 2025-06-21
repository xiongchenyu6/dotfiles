{ pkgs, ... }:
{
  programs = {
    tmux = {
      enable = true;
      terminal = "screen-256color";
      newSession = true;
      disableConfirmationPrompt = true;
      aggressiveResize = true;
      plugins = with pkgs; [
        tmuxPlugins.cpu
        {
          plugin = tmuxPlugins.continuum;
          extraConfig = ''
            set -g @continuum-restore 'on'
            set -g @continuum-save-interval '60' # minutes
          '';
        }
      ];
    };
  };

}
