_:
let
  # Codex reads the same content: shared .codex/skills/* entries in the repo
  # are relative symlinks into .claude/. spec is a Codex-only entry because
  # its loader requires a regular SKILL.md; Claude remains command-only.
  # ~/.codex/skills stays a real directory (it holds manually-linked skills
  # too), so each skill is linked individually.
  codexSkills = [
    "asset-gen"
    "autolife-docs"
    "browser-automation"
    "game-balance-sim"
    "game-polish"
    "kiss-design"
    "long-task-babysit"
    "nixos-deploy"
    "proxy-nodes"
    "spec"
    "token-saving"
    "tui-automation"
  ];
in
{
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
        {
          directory = "ai-skills/.codex/prompts";
          method = "symlink";
        }
      ]
      ++ map (name: {
        directory = "ai-skills/.codex/skills/${name}";
        method = "symlink";
      }) codexSkills;
      files = [ "auth/.authinfo.gpg" ];
    };
  };
}
