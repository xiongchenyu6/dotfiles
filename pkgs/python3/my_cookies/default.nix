{ python3, lib, source, ... }:
let
  buildPythonPackage = python3.pkgs.buildPythonPackage;
  browser-cookie3 = python3.pkgs.browser-cookie3;
in
buildPythonPackage (source.my_cookies // rec {
  propagatedBuildInputs = [ browser-cookie3 ];
  doCheck = false;
})
