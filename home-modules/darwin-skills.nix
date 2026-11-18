{ lib, ... }:
let
  skillTree = ../stow-managed/ai-skills;
  installSkills =
    client:
    let
      root = skillTree + "/.${client}/skills";
      names = builtins.filter (name: builtins.pathExists (root + "/${name}/SKILL.md")) (
        builtins.attrNames (builtins.readDir root)
      );
    in
    lib.listToAttrs (
      map (name: {
        name = ".${client}/skills/${name}";
        value.source = "${skillTree}/.${client}/skills/${name}";
      }) names
    );
in
{
  home.file = installSkills "claude" // installSkills "codex";
}
