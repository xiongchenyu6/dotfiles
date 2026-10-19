# wgmesh on NixOS; membership in shared-modules/wgmesh.nix.
{ inputs, ... }:
{
  imports = [
    inputs.wgmesh.nixosModules.default
    ../shared-modules/wgmesh.nix
    ../shared-modules/wgmesh-keys.nix
  ];
}
