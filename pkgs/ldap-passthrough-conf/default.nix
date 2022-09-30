{ stdenv
, pkgs
, ...
}: stdenv.mkDerivation rec {
  name = "ldap-passthrough-conf";

  src = ./.;
  file = pkgs.writeText "slapd.conf" ''
    pwcheck_method: saslauthd
    saslauthd_path: /run/saslauthd/mux
  '';

  installPhase = ''
    mkdir -p $out;
    cp ${file} $out/slapd.conf;
  '';

}
