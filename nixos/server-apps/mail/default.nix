{ config, pkgs, options, lib, domain, ... }:

{
  imports = [ ./postfix.nix ./dovecot2.nix ./alps.nix ];
}
