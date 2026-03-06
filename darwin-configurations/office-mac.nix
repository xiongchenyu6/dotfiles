# office-mac: host-specific Darwin configuration
{
  inputs,
  ezModules,
  shares,
  ...
}:
{
  imports = [
    ezModules.wireguard
  ];

  nixpkgs.hostPlatform = "aarch64-darwin";
  system.darwinLabel = "gui";

  # Set the primary user for nix-darwin
  system.primaryUser = "freeman.xiong";

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
