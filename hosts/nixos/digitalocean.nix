{ modulesPath, ... }:
{
  imports = [
    ./base.nix
    "${modulesPath}/virtualisation/digital-ocean-image.nix"
  ];
  virtualisation.digitalOceanImage.compressionMethod = "bzip2";
}
