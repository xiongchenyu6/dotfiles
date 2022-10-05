# Edit

{ config, pkgs, options, lib, ... }:
rec {
  imports = [
    # Include the results of the hardware scan.
    ./hardware-configuration.nix
    ../../nixos
    ../../nixos/client.nix
  ];
}
