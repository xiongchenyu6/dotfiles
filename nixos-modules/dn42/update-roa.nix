{ pkgs, ... }:
pkgs.writeShellScriptBin "update-roa" ''
  set -euo pipefail
  mkdir -p /etc/bird/
  ${pkgs.curl}/bin/curl -sfSLR {-o,-z}/etc/bird/roa_dn42_v6.conf https://dn42.burble.com/roa/dn42_roa_bird2_6.conf
  ${pkgs.curl}/bin/curl -sfSLR {-o,-z}/etc/bird/roa_dn42.conf https://dn42.burble.com/roa/dn42_roa_bird2_4.conf
  # bird includes these files, so it cannot start before the first fetch; it
  # picks them up on start. Only a running bird needs an explicit reload.
  if ${pkgs.systemd}/bin/systemctl is-active --quiet bird.service; then
    ${pkgs.bird3}/bin/birdc configure
    ${pkgs.bird3}/bin/birdc reload in all
  fi
''
