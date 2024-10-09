{ config, ... }: {
  sops.secrets."oci-arm-host-capacity" = { };

  services = {
    oci-arm-host-capacity = {
      enable = true;
      envPath = config.sops.secrets."oci-arm-host-capacity".path;
    };
  };

}
