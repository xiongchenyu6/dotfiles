{ stdenv, lib, source, unzip, steam, writeShellScript, ... }:
stdenv.mkDerivation rec {
  name = "tat";
  doCheck = false;
  src = builtins.fetchurl {
    url = "https://tat-gz-1258344699.cos.ap-guangzhou.myqcloud.com/tat_agent_linux_x86_64.zip";
    sha256 = "1payr859rsxllzx2igas1nlnr5lrh2fqzz6gximhii27fv4qh0cv";
  };

  unpackPhase = ''
    ${unzip}/bin/unzip $src
  '';

  installPhase = ''
    mkdir -p $out/bin/out
    cp tat_agent $out/bin
  '';
}
