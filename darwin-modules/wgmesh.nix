# wgmesh on nix-darwin; membership in shared-modules/wgmesh.nix.
{
  config,
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
  # job also starts before /nix/store is mounted and before sops-install-secrets
  # writes the private key, and a failed exit is never retried (KeepAlive only
  # on SuccessfulExit), so wait for both paths first.
  launchd.daemons.wg-quick-wgmesh.serviceConfig.ProgramArguments = lib.mkForce [
    "/bin/sh"
    "-c"
    "/bin/wait4path /nix/store && /bin/wait4path ${lib.escapeShellArg config.services.wgmesh.privateKeyFile} && exec ${pkgs.wireguard-tools}/bin/wg-quick up wgmesh"
  ];
}
