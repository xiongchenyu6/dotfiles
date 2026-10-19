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

  # Own key and address; this used to share the NixOS office host's identity,
  # so only one of the two could be on the mesh at a time.
  sops.secrets."wireguard/office-mac" = { };
  networking.wg-quick.interfaces.wg_ora = {
    privateKeyFile = config.sops.secrets."wireguard/office-mac".path;
    address = [
      "fe80::105/64"
      "172.22.240.105/32"
      "fd48:4b4:f3::9/128"
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
