# wgmesh on nix-darwin; membership in shared-modules/wgmesh.nix.
{
  inputs,
  lib,
  pkgs,
  ...
}:
{
  imports = [
    inputs.wgmesh.darwinModules.default
    ../shared-modules/wgmesh.nix
    ../shared-modules/wgmesh-keys.nix
  ];

  # Launch a system binary rather than a store shebang script: launchd can
  # reject the latter with EX_CONFIG before wg-quick gets to run.
  launchd.daemons.wg-quick-wgmesh.serviceConfig.ProgramArguments = lib.mkForce [
    "/bin/sh"
    "-c"
    "exec ${pkgs.wireguard-tools}/bin/wg-quick up wgmesh"
  ];
}
