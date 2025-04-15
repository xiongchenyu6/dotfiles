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
    ./hardware-configuration.nix
    ezModules.root
    ezModules."freeman.xiong"
    ezModules.core
    ezModules.server
    ezModules.cn
    ezModules.v2ray
    #ezModules.kanidm
    srvos.nixosModules.server
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    rust-web-server.nixosModules.rust-web-server
  ];

  zramSwap.enable = true;

  sops.secrets."rust-web-server/config" = { };

  boot = {
    kernelPackages = lib.mkForce pkgs.linuxPackages_latest;
    tmp.cleanOnBoot = true;
    kernel = {
      sysctl = {
        "net.ipv4.ip_forward" = 1;
        "net.ipv4.conf.default.rp_filter" = 0;
        "net.ipv4.conf.all.rp_filter" = 0;
        "net.ipv4.conf.default.forwarding" = 1;
        "net.ipv4.conf.all.forwarding" = 1;
        "net.ipv6.conf.all.accept_redirects" = 0;
        "net.ipv6.conf.default.forwarding" = 1;
        "net.ipv6.conf.all.forwarding" = 1;
      };
    };
  };

  networking =
    let
      file-path = builtins.split "/" (toString ./.);
      hostName = lib.last file-path;
    in
    {
      inherit hostName;
      firewall = {
        allowedTCPPorts = [
          22
          80
          443
          2222
          7000
          10086
        ];
        allowedUDPPorts = [
          89
          179
          2222
          3478
          4000
          4001
          4002
          7000
          7777
          6696
          33434
        ];
      };
    };

  services = {
    postgresql = {
      enable = true;
      package = pkgs.postgresql_17_jit;
      authentication = ''
        local all all trust
        host  all  all 0.0.0.0/0 scram-sha-256
      '';
      enableTCPIP = true;
      ensureDatabases = [ "rustWebServer" ];
    };

    rust-web-server = {
      enable = true;
      configFile = config.sops.secrets."rust-web-server/config".path;
    };
  };
}
