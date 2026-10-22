# mac-pro: M5 Max MacBook Pro, personal daily driver.
{ config, ezModules, ... }:
{
  imports = [ ezModules.wgmesh ];

  nixpkgs.hostPlatform = "aarch64-darwin";
  system.darwinLabel = "gui";
  system.primaryUser = "freeman.xiong";

  # Static public resolvers: the DHCP DNS here is a China
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
    # Declared so cleanup = "zap" keeps them.
    masApps = {
      Xcode = 497799835;
      WeChat = 836500024;
      WhatsApp = 310633997;
    };
    # Nextcloud's desktop clients (Files, Talk) are not in the App Store; the
    # casks track upstream releases. Calendar/Contacts go through the native apps.
    casks = [
      "nextcloud"
      "nextcloud-talk"
      # Company NetBird: upstream's signed UI; its postflight installs the
      # daemon from the netbird formula as a launchd service. Log in once with
      # `netbird up --management-url <company>`.
      "netbirdio/tap/netbird-ui"
    ];
    brews = [ "netbird" ];
    taps = [ "netbirdio/tap" ];
  };

  # brew bundle runs the netbird-ui postflight with `sudo -E` and no terminal.
  security.sudo.extraConfig = ''
    ${config.system.primaryUser} ALL=(root) NOPASSWD:SETENV: /Applications/Netbird\ UI.app/installer.sh
  '';

  # Stay awake on the charger so the mesh tunnel and ssh keep working; the
  # macOS default puts the laptop to sleep one minute after the display.
  # Battery behaviour is left at the default. The application firewall would
  # drop NetBird peers dialing in directly; brew's netbird is not Apple-signed.
  system.activationScripts.postActivation.text = ''
    pmset -c sleep 0
    if nb=$(readlink -f /opt/homebrew/bin/netbird); then
      /usr/libexec/ApplicationFirewall/socketfilterfw --add "$nb" >/dev/null
      /usr/libexec/ApplicationFirewall/socketfilterfw --unblockapp "$nb" >/dev/null
    fi
  '';

  ids.gids.nixbld = 350;
  users.users."freeman.xiong" = {
    createHome = true;
    description = "Freeman Xiong";
    isHidden = false;
    home = "/Users/freeman.xiong";
  };
}
