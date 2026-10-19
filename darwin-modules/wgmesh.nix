# wgmesh on nix-darwin; membership in shared-modules/wgmesh.nix.
{ inputs, ... }:
{
  imports = [
    inputs.wgmesh.darwinModules.default
    ../shared-modules/wgmesh.nix
    ../shared-modules/wgmesh-keys.nix
  ];
}
