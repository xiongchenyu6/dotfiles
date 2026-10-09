# mac-pro: M5 Max MacBook Pro, personal daily driver.
# WireGuard runs through WireGuard.app (App Store), not the wg-quick module:
# the tunnel private key lives only in that app's config (mesh 172.22.240.103).
_: {
  nixpkgs.hostPlatform = "aarch64-darwin";
  system.darwinLabel = "gui";
  system.primaryUser = "freeman.xiong";

  networking = {
    hostName = "mac-pro";
    computerName = "mac-pro";
    localHostName = "mac-pro";
    knownNetworkServices = [ "Wi-Fi" ];
    dns = [ "1.1.1.1" ];
  };

  ids.gids.nixbld = 350;
  users.users."freeman.xiong" = {
    createHome = true;
    description = "Freeman Xiong";
    isHidden = false;
    home = "/Users/freeman.xiong";
  };
}
