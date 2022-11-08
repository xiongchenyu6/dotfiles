# Edit this configuration file to define what should be installed on

# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  environment.systemPackages = with pkgs; [ krb5 openssh_gssapi litecli ];

  imports = [ ./common.nix ];
  nix.package = pkgs.nix;
  services.nix-daemon.enable = true;
}
