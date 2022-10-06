{ stdenv
, pkgs
, ...
}: stdenv.mkDerivation rec {
  name = "ldap-passthrough-conf";

  src = ./.;
  slapd = pkgs.writeText "slapd.conf" ''
    pwcheck_method: saslauthd
    saslauthd_path: /run/saslauthd/mux
  '';
  smtpd = pkgs.writeText "smtpd.conf" ''
    pwcheck_method: saslauthd
    mech_list: PLAIN LOGIN
    saslauthd_path: /run/saslauthd/mux
    allow_plaintext: true
  '';

  installPhase = ''
    mkdir -p $out;
    cp ${slapd} $out/slapd.conf;
    cp ${smtpd} $out/smtpd.conf;
  '';

}
