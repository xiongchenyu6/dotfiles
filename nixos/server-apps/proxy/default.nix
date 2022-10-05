{ config, pkgs, lib, symlinkJoin, domain, ... }:

{
  imports = [ ./nginx.nix ];
}
