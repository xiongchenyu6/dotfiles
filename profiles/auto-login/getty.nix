{ pkgs, ... }:
{
  services = {
    getty = {
      autologinUser = "freeman.xiong";
    };
  };
}
