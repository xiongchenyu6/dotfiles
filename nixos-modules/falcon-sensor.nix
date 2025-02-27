# Edit
{
  inputs,
  config,
  ...
}:
{
  imports = with inputs; [
    xiongchenyu6.nixosModules.falcon-sensor
  ];
  sops.secrets."falcon/cid" = { };

  services = {
    falcon-sensor = {
      enable = true;
      cidFile = config.sops.secrets."falcon/cid".path;
      traceLevel = "debug";
    };
  };
}
