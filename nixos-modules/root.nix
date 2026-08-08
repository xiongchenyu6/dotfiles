{
  pkgs,
  lib,
  shares,
  ...
}:
{

  users.mutableUsers = lib.mkDefault false;

  programs.zsh.enable = true;
  users.users.root = {
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys = [
      shares.users."freeman.xiong".public-key
      shares.users."freeman.xiong".yubikey
    ];
  };

}
