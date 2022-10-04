{ config, pkgs, lib, symlinkJoin, ... }:
{
  services =
    {
      openldap =
        {
          enable = true;
        };
    };
}
