{
  inputs,
  modulesPath,
  lib,
  pkgs,
  config,
  ezModules,
  ...
}:
{
  imports = with inputs; [
    disko.nixosModules.disko
    (modulesPath + "/installer/scan/not-detected.nix")
    (modulesPath + "/profiles/qemu-guest.nix")
    ezModules.root
    ezModules."freeman.xiong"
    ezModules.core
    ezModules.server
    ezModules.acme
    ezModules.datadog-agent
    ezModules.sing-box
    srvos.nixosModules.server
    ezModules.mixins-nginx
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
    ./hardware-configuration.nix
  ];

  services = {
    openssh = {
      enable = true;
      authorizedKeysCommand = "/run/wrappers/bin/kanidm_ssh_authorizedkeys %u";
      authorizedKeysCommandUser = "nobody";
      settings = {
        UsePAM = true;
      };
    };
  };
}
