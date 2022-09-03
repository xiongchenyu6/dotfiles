{ python3, lib }:
let buildPythonPackage = python3.pkgs.buildPythonPackage;
    fetchPypi = python3.pkgs.fetchPypi;
    browser-cookie3 = python3.pkgs.browser-cookie3;
in
buildPythonPackage rec {
  pname = "my_cookies";
  version = "0.1.3";
  propagatedBuildInputs = [ browser-cookie3 ];

  src = fetchPypi {
      inherit pname version;
      sha256 = "sha256-3e5j0HFOXUyUo6YVUKQnbaxvAUtDoRTzGqW8HUfzrQ8="; # replace with actual sha after trying to build
  };
  doCheck = false;
}
