# wgmesh on NixOS; the settings live in shared-modules/wgmesh.nix.
{ inputs, ... }:
{
  imports = [
    inputs.wgmesh.nixosModules.default
    ../shared-modules/wgmesh.nix
  ];
}
