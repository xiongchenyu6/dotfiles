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
    openssh.authorizedKeys.keys = lib.mkDefault [
      shares.users-dict."freeman.xiong".public-key
    ];
  };

}
