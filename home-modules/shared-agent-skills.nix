{ lib, ... }:
let
  skillTree = ../stow-managed/ai-skills/.claude/skills;
  portableSkills = [
    "git-sync"
    "nixos-disk-cleanup"
  ];
  installSkills =
    directory:
    lib.listToAttrs (
      map (name: {
        name = "${directory}/${name}";
        value.source = skillTree + "/${name}";
      }) portableSkills
    );
in
{
  home.file =
    # OpenCode already discovers the shared skills through Claude’s directory.
    installSkills ".pi/agent/skills"
    // installSkills ".gemini/config/skills";
}
