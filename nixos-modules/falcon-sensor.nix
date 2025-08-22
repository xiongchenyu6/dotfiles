# Edit
{
  inputs,
  config,
  lib,
  ...
}:
{
  imports = with inputs; [
    xiongchenyu6.nixosModules.falcon-sensor
  ];
  sops.secrets."falcon/cid" = { };

  # Allow unfree packages including falcon-sensor
  nixpkgs.config = {
    allowUnfree = true;
    allowUnfreePredicate = _: true;
  };

  services = {
    falcon-sensor = {
      enable = true;
      cidFile = config.sops.secrets."falcon/cid".path;
      traceLevel = "debug";
    };
  };
}
