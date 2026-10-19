# wgmesh on nix-darwin; the settings live in shared-modules/wgmesh.nix.
{ inputs, ... }:
{
  imports = [
    inputs.wgmesh.darwinModules.default
    ../shared-modules/wgmesh.nix
  ];
}
