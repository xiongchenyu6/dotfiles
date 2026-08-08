_: {
  home = {
    persistence."/home/freeman.xiong/dotfiles/stow-managed/" = {
      removePrefixDirectory = true;
      allowOther = false;
      directories = [
        "config/.config/xmonad"
        #"config/.config/nvim"
        "password-store/.local/share/password-store"
        "albert/.local/share/albert"
        "config/.config/albert"
        "config/.config/Code"
        "rime/.local/share/fcitx5/rime"
        # Personal AI skills/prompts, maintained in the repo and linked into
        # ~/.claude so Claude Code picks them up in every project. Symlink
        # method (not bindfs) so edits in the repo are live immediately.
        {
          directory = "ai-skills/.claude/skills";
          method = "symlink";
        }
        {
          directory = "ai-skills/.claude/commands";
          method = "symlink";
        }
      ];
      files = [ "auth/.authinfo.gpg" ];
    };
  };
}
