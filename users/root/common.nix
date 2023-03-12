# Edit this configuration file to define what should be installed on
{ hmUsers, pkgs, ... }: {
  users.users.root = { shell = pkgs.zsh; };
  home-manager.users = { inherit (hmUsers) root; };
}
