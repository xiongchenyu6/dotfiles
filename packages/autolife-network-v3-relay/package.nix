# Prebuilt AutoLife Network-V3 relay binary compiled natively on oracle-arm-001
# (aarch64-linux) from pinned AutoLifeRobot/AutolifeNetwork-V3 source revision,
# then imported into the local Nix store with `nix-store --add-fixed sha256`.
{
  lib,
  stdenvNoCC,
  requireFile,
  autoPatchelfHook,
  stdenv,
}:
let
  artifact = builtins.fromJSON (builtins.readFile ./artifact.json);
in
assert artifact.system == "aarch64-linux";
stdenvNoCC.mkDerivation {
  pname = "autolife-network-v3-relay";
  version = builtins.substring 0 12 artifact.sourceRevision;
  src = requireFile {
    inherit (artifact) name sha256;
    message = ''
      Build the pinned Network-V3 source natively on oracle-arm-001 (aarch64-linux)
      and import it with:
        nix-store --add-fixed sha256 /var/lib/autolife-artifacts/${artifact.name}
    '';
  };
  nativeBuildInputs = [ autoPatchelfHook ];
  buildInputs = [ stdenv.cc.cc.lib ];
  dontUnpack = true;
  dontStrip = true;
  installPhase = ''
    runHook preInstall
    install -Dm755 "$src" "$out/bin/autolife-relay-server"
    runHook postInstall
  '';
  meta = {
    description = "AutoLife Network V3 authenticated UDP ciphertext relay";
    platforms = [ "aarch64-linux" ];
    license = lib.licenses.unfree;
    mainProgram = "autolife-relay-server";
  };
}
