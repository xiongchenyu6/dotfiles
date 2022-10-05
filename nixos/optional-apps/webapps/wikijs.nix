{ config, pkgs, options, lib, ... }:

{
  services = {
    wiki-js = {
      enable = true;
      settings = {
        port = 3456;
        db = {
          host = "127.0.0.1";
          port = 5432;
          user = "wikijs";
          type = "postgres";
          pass = "wikijs";
        };
        logLevel = "debug";

      };
      environmentFile = ../../common/env;
    };

  };
}
