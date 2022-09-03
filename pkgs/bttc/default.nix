{ stdenv, fetchFromGitHub, buildGoModule, lib }:
let bin = "bttc";
in buildGoModule rec {
  name = "bttc-${version}";
  version = "v1.0.1";
  src = fetchFromGitHub ({
    owner = "bttcprotocol";
    repo = "bttc";
    rev = "${version}";
    sha256 = "sha256-116iZ5kOJsrnDrUAq34HntvkcJQXzMbqYtey3jgeLLA=";
  });
  enableParallelBuilding = true;
  proxyVendor = true;
  vendorSha256 = "sha256-GsnUb3D5JTZnNhUssLDDJaLGkD6z9qjSDQz9ahcKGmo=";
  subPackages = [ "cmd/geth" ];
  doCheck = false;
  postInstall = "mv $out/bin/geth $out/bin/${bin}";
  meta = with lib; {
    homepage = "https://www.bttc.com/";
    description = "Official golang implementation of the Bttc protocol";
    license = with licenses; [ lgpl3Plus gpl3Plus ];
    maintainers = with maintainers; [ adisbladis lionello RaghavSood ];
  };
}
