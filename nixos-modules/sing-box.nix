{
  ...
}:
{
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
                uuid = builtins.getEnv "V2RAY";
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
