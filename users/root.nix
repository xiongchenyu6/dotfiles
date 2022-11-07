# Edit this configuration file to define what should be installed on

# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
let
  common-files-path = ../common;
  share = import (common-files-path + /share.nix);

in {
  users.users.root = {
    openssh.authorizedKeys.keys = [ share.office.user.public-key ];
  };
}
