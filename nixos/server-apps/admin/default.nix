{ config, pkgs, options, lib, ... }:

{

  imports = [
    ./sasl.nix
    ./openldap.nix
    ./kerberos.nix
    ./sssd.nix
  ];
}
