{
  config,
  ...
}:
{
  # Define sops secret for V2RAY UUID
  sops.secrets."sing-box/V2RAY" = {
    owner = "sing-box";
    mode = "0400";
  };

  networking = {
    firewall = {
      allowedTCPPorts = [
        10086
      ];
    };
  };

  services = {
    sing-box = {
      enable = true;
      settings = {
        inbounds = [
          {
            type = "vless";
            listen = "::";
            listen_port = 10086;
            users = [
              {
                uuid = {
                  _secret = config.sops.secrets."sing-box/V2RAY".path;
                };
                flow = "";
              }
            ];
          }
        ];
        outbounds = [
          {
            type = "direct";
            tag = "direct";
          }
        ];
      };
    };
  };
}
