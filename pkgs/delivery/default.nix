{ stdenv, fetchFromGitHub, buildGoModule, lib, pkgs }:

let bins = {
      heimdalld = "deliveryd";
      heimdallcli = "deliverycli";
    };
in buildGoModule rec {
  name = "delivery-${version}";
  version = "v1.0.1";
  src = fetchFromGitHub ({
    owner = "bttcprotocol";
    repo = "delivery";
    rev = "${version}";
    sha256 = "sha256-BOEqMCpfQ4CitdmzbaLLtTK+o6t+W5stR36zm3KccCA=";
  });
  enableParallelBuilding = true;
  proxyVendor = true;
  vendorSha256 = "sha256-Y6HR/sIcXzuIkvk6POH/wGq2jtPoLpBmUBRsAfpUxRg=";

  ldflags = [
    "-X github.com/bttcprotocol/delivery/version.Name=delivery"
    "-X github.com/bttcprotocol/delivery/version.ServerName=deliveryd"
    "-X github.com/bttcprotocol/delivery/version.ClientName=deliverycli"
    "-X github.com/bttcprotocol/delivery/version.Commit=a3d6186d67767658bff98b866b0e56f252f4b44c"
    "-X github.com/cosmos/cosmos-sdk/version.Name=delivery"
    "-X github.com/cosmos/cosmos-sdk/version.ServerName=deliveryd"
    "-X github.com/cosmos/cosmos-sdk/version.ClientName=deliverycli"
    "-X github.com/cosmos/cosmos-sdk/version.Commit=a3d6186d67767658bff98b866b0e56f252f4b44c"
  ];
  subPackages = [ "cmd/heimdalld" "cmd/heimdallcli" "bridge/bridge.go" ];
  postInstall = lib.concatStringsSep "\n" (lib.attrValues (lib.mapAttrs (name: value:"mv $out/bin/${name} $out/bin/${value}") bins));
 
  meta = with lib; {
    homepage = "https://www.bttc.com/";
    description = "Official golang implementation of the Bttc protocol";
    license = with licenses; [ lgpl3Plus gpl3Plus ];
    maintainers = with maintainers; [ adisbladis lionello RaghavSood ];
  };
}

