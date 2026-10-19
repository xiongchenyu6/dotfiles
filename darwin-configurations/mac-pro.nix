# mac-pro: M5 Max MacBook Pro, personal daily driver.
{ ezModules, ... }:
{
  imports = [ ezModules.wgmesh ];

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

  # Route-monitor recalculation would otherwise raise the tunnel MTU.
  networking.wg-quick.interfaces.wgmesh.mtu = 1280;

  homebrew = {
    masApps.Xcode = 497799835;
    # Nextcloud's desktop clients (Files, Talk) are not in the App Store; the
    # casks track upstream releases. Calendar/Contacts go through the native apps.
    casks = [
      "nextcloud"
      "nextcloud-talk"
    ];
  };

  # Stay awake on the charger so the mesh tunnel and ssh keep working; the
  # macOS default puts the laptop to sleep one minute after the display.
  # Battery behaviour is left at the default.
  system.activationScripts.postActivation.text = ''
    pmset -c sleep 0
  '';

  ids.gids.nixbld = 350;
  users.users."freeman.xiong" = {
    createHome = true;
    description = "Freeman Xiong";
    isHidden = false;
    home = "/Users/freeman.xiong";
  };
}
