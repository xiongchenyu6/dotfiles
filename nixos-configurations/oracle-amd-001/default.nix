{
  config,
  inputs,
  modulesPath,
  lib,
  pkgs,
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
    srvos.nixosModules.server
    #srvos.nixosModules.mixins-nginx
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
    ./hardware-configuration.nix
  ];

  sops.secrets."cloudflared/tunnel-credentials" = { };

  environment = {
    systemPackages = with pkgs; [
      kanidm
      cloudflared
    ];
  };

  networking = {
    firewall = {
      allowedTCPPorts = [
        80
        443
        636
      ];
      allowedUDPPorts = [ 53 ]; # Added for DNS
    };
  };

  security = {
    acme = {
      certs = {
        ${config.networking.domain} = {
          domain = "${config.networking.domain}";
          extraDomainNames = [ "*.${config.networking.domain}" ];
          group = "kanidm";
        };
      };
    };
  };

  services = {
    cloudflared = {
      enable = true;
      tunnels = {
        "31881776-71bc-4c81-b206-b579ca61ffd2" = {
          credentialsFile = config.sops.secrets."cloudflared/tunnel-credentials".path;
          # Default route for unmatched requests
          warp-routing = {
            enabled = true;
          };
          default = "http_status:404";
          # Add specific routes here as needed, for example:
          # "kanidm.${config.networking.domain}" = "localhost:443";
        };
      };
    };
  };
}
