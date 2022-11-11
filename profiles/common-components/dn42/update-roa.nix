{pkgs, ...}:
pkgs.writeShellScriptBin "update-roa" ''
  mkdir -p /etc/bird/
  ${pkgs.curl}/bin/curl -sfSLR {-o,-z}/etc/bird/roa_dn42_v6.conf https://dn42.burble.com/roa/dn42_roa_bird2_6.conf
  ${pkgs.curl}/bin/curl -sfSLR {-o,-z}/etc/bird/roa_dn42.conf https://dn42.burble.com/roa/dn42_roa_bird2_4.conf
  ${pkgs.bird2}/bin/birdc c
  ${pkgs.bird2}/bin/birdc reload in all
''
