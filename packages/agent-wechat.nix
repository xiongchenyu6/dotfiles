{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
  openssl,
}:
let
  assets = {
    aarch64-darwin = {
      name = "wx-v2.3.1-darwin-arm64";
      hash = "sha256-x7QQIKGK+9amfAAgZmBDmLZkpwXT2pWrsc2Ak1NGslE=";
    };
    aarch64-linux = {
      name = "wx-v2.3.1-linux-arm64-gnu";
      hash = "sha256-6FnSPHB0/CaZx+FiP5xdRPnYtjpbT8HdOs9IhYhzeXI=";
    };
    x86_64-linux = {
      name = "wx-v2.3.1-linux-x64-gnu";
      hash = "sha256-0Fd2Fu4neiqMlnHMhqy16fAmoakp5PLMrnK36IdeP7E=";
    };
  };
  asset = assets.${stdenv.hostPlatform.system};
in
stdenv.mkDerivation {
  pname = "agent-wechat";
  version = "2.3.1";
  src = fetchurl {
    url = "https://github.com/thisnick/agent-wechat/releases/download/v2.3.1/${asset.name}";
    inherit (asset) hash;
  };
  dontUnpack = true;
  dontBuild = true;
  dontStrip = true;
  nativeBuildInputs = lib.optionals stdenv.hostPlatform.isLinux [ autoPatchelfHook ];
  buildInputs = lib.optionals stdenv.hostPlatform.isLinux [
    stdenv.cc.cc.lib
    openssl
  ];
  installPhase = ''
    runHook preInstall
    install -Dm755 $src $out/bin/wx
    runHook postInstall
  '';
  meta = {
    description = "Independent WeChat client and local API (wx CLI)";
    homepage = "https://github.com/thisnick/agent-wechat";
    license = lib.licenses.mit;
    platforms = builtins.attrNames assets;
    mainProgram = "wx";
  };
}
