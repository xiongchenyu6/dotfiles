# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  pkgs,
  config,
  lib,
  ...
}: {
  sops.secrets."zammad/secret-key-base" = {
    owner = "zammad";
    group = "zammad";
  };
  services = {
    zammad = {
      port = 3005;
      enable = true;
      openPorts = true;
      secretKeyBaseFile = config.sops.secrets."zammad/secret-key-base".path;
    };
  };
}
