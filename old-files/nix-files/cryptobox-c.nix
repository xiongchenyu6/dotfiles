with import <nixpkgs> { };
rustPlatform.buildRustPackage rec {
  name = "cryptobox-c";
  version = "0.4.0";

  src = fetchFromGitHub {
    owner = "wireapp";
    repo = "cryptobox-c";
    rev = "9a9df46e61a897ca57f1ab53badf7ad103263b06";
    sha256 = "06s4c4sfxx8mr53jivvd7qnq75b9rb9a7k9qsy0hz0746wrdsa91";
  };

  cargoSha256 = "1m6d0bp8520azq8ph3yzv1zc8bg3jvd3van007kamqyggcj2r7f6";

  buildInputs = [ cargo pkgconfig libsodium clang ];
  # Set Environment Variables
  RUST_BACKTRACE = 1;
  meta = with stdenv.lib; {
    description =
      "A fast line-oriented regex search tool, similar to ag and ack";
    license = licenses.unlicense;
    maintainers = [ maintainers.tailhook ];
    platforms = platforms.all;
  };

  fixupPhase = ''
    mkdir -p $out/include
    cp ${src}/src/cbox.h $out/include
  '';
}
