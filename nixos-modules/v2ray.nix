{
  pkgs,
  config,
  lib,
  ...
}:
{
  services = {
    v2ray = {
      enable = true;
      config = {
        inbounds = [
          {
            port = 10086;
            protocol = "vless";
            settings = {
              clients = [
                {
                  id = builtins.getEnv "V2RAY";
                  alterId = 64;
                }
              ];
              decryption = "none"; # Add this line
            };
          }
        ];
        outbounds = [
          {
            protocol = "freedom";
            settings = { };
          }
        ];
      };
    };
  };
}
