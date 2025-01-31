{
  pkgs,
  lib,
  shares,
  ...
}:
{

  users.mutableUsers = lib.mkDefault false;

  users.users.root = {
    shell = pkgs.zsh;
    openssh.authorizedKeys.keys = lib.mkDefault [
      shares.users-dict."freeman.xiong".public-key
    ];
  };

}
