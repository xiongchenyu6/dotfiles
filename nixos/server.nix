{ config, pkgs, lib, symlinkJoin, domain, ... }:
{
  imports =
    let
      ls = dir: builtins.map (f: (dir + "/${f}")) (builtins.attrNames (builtins.readDir dir));
    in
    [ ]
    ++ (ls ./common-apps)
    ++ (ls ./server-apps)
    ++ (ls ./common-components)
    ++ (ls ./server-components)
  ;
  services =
    { };

}