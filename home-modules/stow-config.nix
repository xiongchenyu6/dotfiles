_: {
  home = {
    persistence."/home/freeman.xiong/dotfiles/stow-managed/" = {
      removePrefixDirectory = true;
      allowOther = false;
      directories = [
        #"config/.config/nvim"
        "password-store/.local/share/password-store"
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
        # Codex reads the same content: .codex/prompts and .codex/skills/* in
        # the repo are relative symlinks into .claude/, so both tools share
        # one source of truth. ~/.codex/skills stays a real directory (it
        # holds manually-linked skills too), so each skill links individually.
        {
          directory = "ai-skills/.codex/prompts";
          method = "symlink";
        }
        {
          directory = "ai-skills/.codex/skills/game-polish";
          method = "symlink";
        }
        {
          directory = "ai-skills/.codex/skills/nixos-deploy";
          method = "symlink";
        }
        {
          directory = "ai-skills/.codex/skills/autolife-docs";
          method = "symlink";
        }
        {
          directory = "ai-skills/.codex/skills/browser-automation";
          method = "symlink";
        }
        {
          directory = "ai-skills/.codex/skills/token-saving";
          method = "symlink";
        }
        {
          directory = "ai-skills/.codex/skills/tui-automation";
          method = "symlink";
        }
        {
          directory = "ai-skills/.codex/skills/comfyui-assets";
          method = "symlink";
        }
        {
          directory = "ai-skills/.codex/skills/long-task-babysit";
          method = "symlink";
        }
        # 自写的 noctalia launcher 插件(>nx 搜 nixpkgs);市场插件仍在
        # ~/.config/noctalia/plugins 本地,只有这个是仓库管理的
        {
          directory = "noctalia/.config/noctalia/plugins/nix-search";
          method = "symlink";
        }
        {
          directory = "noctalia/.config/noctalia/plugins/web-search";
          method = "symlink";
        }
        {
          directory = "noctalia/.config/noctalia/plugins/ticker";
          method = "symlink";
        }
      ];
      files = [ "auth/.authinfo.gpg" ];
    };
  };
}
