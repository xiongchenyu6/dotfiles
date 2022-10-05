{ config, pkgs, lib, symlinkJoin, domain, ... }:

{
  services =
    {
      postfix = {
        inherit domain;
        enable = true;
      };
    };
}
