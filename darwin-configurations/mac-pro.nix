# mac-pro: M5 Max MacBook Pro, personal daily driver.
# WireGuard runs through WireGuard.app (App Store), not the wg-quick module:
# the tunnel private key lives only in that app's config (mesh 172.22.240.103).
_: {
  nixpkgs.hostPlatform = "aarch64-darwin";
  system.darwinLabel = "gui";
  system.primaryUser = "freeman.xiong";

  # Static public resolvers like office-mac: the DHCP DNS here is a China
  # Mobile resolver that NXDOMAINs brew cask hosts and breaks TLS to
  # itunes.apple.com, so `mas` (App Store installs) cannot even look apps up.
  networking = {
    hostName = "mac-pro";
    computerName = "mac-pro";
    localHostName = "mac-pro";
    knownNetworkServices = [ "Wi-Fi" ];
    dns = [
      "1.1.1.1"
      "8.8.8.8"
    ];
  };

  homebrew.masApps = {
    WireGuard = 1451685025;
    Xcode = 497799835;
  };

  ids.gids.nixbld = 350;
  users.users."freeman.xiong" = {
    createHome = true;
    description = "Freeman Xiong";
    isHidden = false;
    home = "/Users/freeman.xiong";
  };
}
