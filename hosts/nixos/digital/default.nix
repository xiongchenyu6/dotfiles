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

  sops.secrets."wireguard/digital" = { };
  sops.secrets."authentik/env" = { };
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
    redis = {
      servers.authentik = {
        enable = lib.mkForce false;
      };
    };

    authentik = {
      enable = true;
      createDatabase = false;
      # The environmentFile needs to be on the target host!
      # Best use something like sops-nix or agenix to manage it
      environmentFile = config.sops.secrets."authentik/env".path;
      settings = {
        disable_startup_analytics = true;
        avatars = "initials";
      };
    };
  };
}
