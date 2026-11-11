# Use upstream releases: tg embeds its public application credentials at release time.
{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
}:
let
  mkRelease =
    {
      pname,
      version,
      repo,
      description,
      assets,
    }:
    let
      asset = assets.${stdenv.hostPlatform.system};
    in
    stdenv.mkDerivation {
      inherit pname version;
      src = fetchurl {
        url = "https://github.com/${repo}/releases/download/v${version}/${pname}_${version}_${asset.archive}.tar.gz";
        inherit (asset) hash;
      };
      sourceRoot = ".";
      nativeBuildInputs = lib.optionals stdenv.hostPlatform.isLinux [ autoPatchelfHook ];
      buildInputs = lib.optionals stdenv.hostPlatform.isLinux [ stdenv.cc.cc.lib ];
      dontBuild = true;
      # Preserve the upstream macOS signature and Go executable.
      dontStrip = true;
      installPhase = ''
        runHook preInstall
        install -Dm755 ${pname} $out/bin/${pname}
        runHook postInstall
      '';
      meta = {
        inherit description;
        homepage = "https://github.com/${repo}";
        license = lib.licenses.mit;
        platforms = builtins.attrNames assets;
        mainProgram = pname;
      };
    };
in
{
  wacli = mkRelease {
    pname = "wacli";
    version = "0.26.0";
    repo = "openclaw/wacli";
    description = "WhatsApp command-line client";
    assets = {
      aarch64-darwin = {
        archive = "darwin_arm64";
        hash = "sha256-Ueoh6+zEfM1d9r2QlA/oJ/wOseGRCYv1bdAzY9adNKU=";
      };
      x86_64-darwin = {
        archive = "darwin_amd64";
        hash = "sha256-/YeqGIc4hjdrCotPpDxKwcR+BHF3pVn6S2rdWUNxczo=";
      };
      aarch64-linux = {
        archive = "linux_arm64";
        hash = "sha256-d3pdzOzhJCG37krwAbB05evBx6Y/cyz2QsPns8TXx7k=";
      };
      x86_64-linux = {
        archive = "linux_amd64";
        hash = "sha256-LCj2Xi+arzJYUoginbH5FsbF+125UTcVAX1ShRNgS0Q=";
      };
    };
  };
  tg = mkRelease {
    pname = "tg";
    version = "0.11.0";
    repo = "gotd/cli";
    description = "Telegram command-line client";
    assets = {
      aarch64-darwin = {
        archive = "darwin_arm64";
        hash = "sha256-nnKwmQPGngo4VN/axyK9RLmdTy9blyHii/H6IB8rYvc=";
      };
      x86_64-darwin = {
        archive = "darwin_amd64";
        hash = "sha256-rOfBInlgU2Yngc5g5zaTDrTCXDY8eAFAeqNWbRXNy0o=";
      };
      aarch64-linux = {
        archive = "linux_arm64";
        hash = "sha256-0m8RvirfwwyanxCqjykw1zb4NGj0Ur+BSWOx2RfFR0s=";
      };
      x86_64-linux = {
        archive = "linux_amd64";
        hash = "sha256-NRD8ulWq3qLKG2MHZtN/ttujDEqySfnTrazGDKddQ8g=";
      };
    };
  };
}
