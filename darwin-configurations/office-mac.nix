# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  inputs,
  ezModules,
  shares,
  ...
}: {
  imports = [
    ezModules.wireguard
  ];
  ids.gids.nixbld = 350;
  users = {
    users = {
      "freeman.xiong" = {
        createHome = true;
        description = "Freeman Xiong";
        isHidden = false;
        home = "/Users/freeman.xiong";
      };
    };
  };
}
