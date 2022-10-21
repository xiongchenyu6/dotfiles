{ config, pkgs, lib, ... }:

{
  imports = [ ./grafana.nix ./prometheus.nix ./endlessh-go.nix ];
}
