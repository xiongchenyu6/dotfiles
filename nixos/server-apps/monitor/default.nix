{ config, pkgs, lib, ... }:

{
  imports = [ ./grafana.nix ./prometheus.nix ];
}
