# Edit this configuration file to define what should be installed on

# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, lib, ... }:

{
  environment = { systemPackages = with pkgs; [ python3 calibre xvfb-run ]; };

  services = {
    oci-arm-host-capacity =
      let envPath = ../common/oci-arm-host-capacity.secret;
      in {
        enable = true;
        envPath = "${envPath}";
      };
  };
}
