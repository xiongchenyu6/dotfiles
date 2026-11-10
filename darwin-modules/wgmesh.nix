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
  # reject the latter with EX_CONFIG before wg-quick gets to run. At boot the
  # job also starts before the /nix/store volume is mounted, and a failed exit
  # is never retried (KeepAlive only on SuccessfulExit), so wait for the store
  # first, as nix-darwin's own `command` option does.
  launchd.daemons.wg-quick-wgmesh.serviceConfig.ProgramArguments = lib.mkForce [
    "/bin/sh"
    "-c"
    "/bin/wait4path /nix/store && exec ${pkgs.wireguard-tools}/bin/wg-quick up wgmesh"
  ];
}
