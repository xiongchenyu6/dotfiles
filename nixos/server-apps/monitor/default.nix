{ config, pkgs, lib, symlinkJoin, domain, ... }:

{
  imports = [ ./grafana.nix ./prometheus.nix ];
}
