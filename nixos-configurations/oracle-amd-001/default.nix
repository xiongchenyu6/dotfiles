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

  environment = {
    systemPackages = with pkgs; [
      kanidm
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
          domain = "auto-life.tech";
          extraDomainNames = [ "*.auto-life.tech" ];
          group = "kanidm";
        };
      };
    };
  };

  services = {
    kanidm =
      let
        credsDir = config.security.acme.certs."${config.networking.domain}".directory;
      in
      {
        enableServer = true;
        enablePam = true;
        clientSettings = {
          uri = "https://kanidm.auto-life.tech";
        };
        unixSettings = {
          default_shell = "${pkgs.zsh}/bin/zsh";
          home_alias = "name";
          home_attr = "uuid";
          pam_allowed_login_groups = [ "devops" ]; # Updated group to match changes in groups
        };

        serverSettings = {
          tls_key = credsDir + "/key.pem";
          tls_chain = credsDir + "/cert.pem";
          origin = "https://kanidm.auto-life.tech";
          domain = "kanidm.auto-life.tech";
          bindaddress = "0.0.0.0:443";
          online_backup.versions = 3;
        };
        provision = {
          enable = true;
          groups = {
            devops = {
              members = [ "xiongchenyu" ];
            };
          };
          persons = {
            xiongchenyu = {
              mailAddresses = [ "xiongchenyu6@gmail.com" ];
              legalName = "Xiong Chenyu";
              displayName = "Xiong Chenyu";
              groups = [ "devops" ]; # Updated group to match changes in groups
            };
          };
        };
      };
  };

}
