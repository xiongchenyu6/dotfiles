{ config, pkgs, lib, ... }: {
  imports = let
    ls = dir:
      builtins.map (f: (dir + "/${f}"))
      (builtins.attrNames (builtins.readDir dir));
  in [ ] ++ (ls ./common-apps) ++ (ls ./server-apps) ++ (ls ./common-components)
  ++ (ls ./server-components);
  services = {
    oci-arm-host-capacity =
      let envPath = ../common/oci-arm-host-capacity.secret;
      in {
        enable = true;
        envPath = "${envPath}";
      };
  };
}
