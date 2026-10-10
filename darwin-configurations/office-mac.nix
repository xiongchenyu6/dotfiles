# office-mac: host-specific Darwin configuration
{
  config,
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

  networking.dns = [ "1.1.1.1" ];

  sops.secrets."wireguard/office" = { };
  networking.wg-quick.interfaces.wg_ora = {
    privateKeyFile = config.sops.secrets."wireguard/office".path;
    address = [
      "fe80::101/64"
      "172.22.240.98/32"
      "fd48:4b4:f3::2/128"
    ];
  };

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
