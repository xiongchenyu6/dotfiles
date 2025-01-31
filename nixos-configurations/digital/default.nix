{
  modulesPath,
  inputs,
  lib,
  ezModules,
  config,
  pkgs,
  shares,
  ...
}:
{
  imports = with inputs; [
    "${modulesPath}/installer/scan/not-detected.nix"
    ./hardware-configuration.nix
    ezModules.root
    ezModules."freeman.xiong"
    ezModules.core
    ezModules.server
    ezModules.sops
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    vscode-server.nixosModules.default
  ];

  sops.secrets."wireguard/digital" = { };
  sops.secrets."authentik/env" = { };
  
  boot.loader.grub = {
    enable = true;
    devices = [ "/dev/vda" ];
    efiSupport = true;
    efiInstallAsRemovable = true;
  };

  networking =
    let
      file-path = builtins.split "/" (toString ./.);
      hostName = lib.last file-path;
    in
    {
      useDHCP = lib.mkForce false;
      inherit hostName;
      firewall = {
        allowedTCPPorts = [
          22
          89
          179
          2222
        ];
        allowedUDPPorts = [
          89
          179
          2222
          6696
          33434
        ];
      };
    };

  services = {
    do-agent = {
      enable = true;
    };
  };

  home-manager = {
    users = {
      "freeman.xiong" = {
        programs = {
          tmux.enable = true;
        };
      };
    };
  };
}
