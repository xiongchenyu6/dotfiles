{ config, pkgs, options, lib, ... }:

{
  services = {
    cachix-agent = {
      enable = true;
      credentialsFile = ../../common/cachix.secret;
      verbose = true;
    };
  };
}

