{ python3, lib, source, ... }:
let
  inherit (python3.pkgs) buildPythonPackage;
  inherit (python3.pkgs) browser-cookie3;
in
buildPythonPackage (source.my_cookies // rec {
  propagatedBuildInputs = [ browser-cookie3 ];
  doCheck = false;
})
