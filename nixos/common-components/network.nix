{ config, pkgs, options, lib, ... }:
let share = import ../../common/share.nix;

in {
  networking = { domain = "freeman.engineer"; };

}
