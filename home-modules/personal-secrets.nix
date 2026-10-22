# Declared in its own module at the lowest tier so every consumer sees it,
# including the company flake, which imports homeModules.workstation without
# ever loading home-modules/default.nix.
{ lib, ... }:
{
  options.dotfiles.personalSecrets = lib.mkEnableOption "personal workstation secrets";
}
