{
  profiles,
  config,
  lib,
  pkgs,
  ...
}:
{

  imports = [
    ../../../users/root/nixos.nix
    ../../../users/freeman.xiong
    ../../../profiles/sops.nix
    ../../../profiles/core/nixos.nix
    ../../../profiles/server/components
    ../../../profiles/common/components
    #../../../profiles/common/components/datadog-agent.nix
    ./hardware-configuration.nix
    ./networking.nix
  ];

  boot = {
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
        enable = false;
      };
    };
  services = {
    openssh = {
      openFirewall = true;
    };
    do-agent = {
      enable = true;
    };
    netbird = {
      server = {
        signal = {
          enable = true;
          port = 6696;
          enableNginx = true;
          domain = "netbird.autolife-robotics.me";
        };
        coturn = {
          enable = true;
          port = 33434;
        };
        dashboard = {
          enable = true;
          domain = "netbird.autolife-robotics.me";
          managementServer = "https://netbird.autolife-robotics.me";
          settings = "{}";
        };
      };
    };
  };
}
