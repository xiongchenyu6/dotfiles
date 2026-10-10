# mac-pro: M5 Max MacBook Pro, personal daily driver.
{ config, ezModules, ... }:
{
  imports = [ ezModules.wireguard ];

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

  # Mesh peer of the oracle-amd-002 hub (public key in shares.toml).
  sops.secrets."wireguard/mac-pro" = { };
  networking.wg-quick.interfaces.wg_ora = {
    privateKeyFile = config.sops.secrets."wireguard/mac-pro".path;
    address = [
      "fe80::107/64"
      "172.22.240.103/32"
      "fd48:4b4:f3::7/128"
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
