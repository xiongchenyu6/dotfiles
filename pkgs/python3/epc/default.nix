{ python3, lib, source, ... }:
let
  inherit (python3.pkgs) buildPythonPackage;
  inherit (python3.pkgs) sexpdata;
in
buildPythonPackage (source.epc // rec {
  propagatedBuildInputs = [ sexpdata ];
  doCheck = false;
})
