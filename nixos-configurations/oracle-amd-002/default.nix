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
    xiongchenyu6.nixosModules.sub2api
    srvos.nixosModules.server
    ezModules.mixins-nginx
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
    ./hardware-configuration.nix
  ];
  # security.wrappers.kanidm_ssh_authorizedkeys = {
  #   owner = "root";
  #   group = "root";
  #   source = "${pkgs.kanidm}/bin/kanidm_ssh_authorizedkeys";
  # };

  security.acme.certs."xiongchenyu.dpdns.org" = {
    domain = "xiongchenyu.dpdns.org";
    extraDomainNames = [ "*.xiongchenyu.dpdns.org" ];
    group = "nginx";
  };

  sops.secrets."sub2api/env" = {
    owner = "sub2api";
    group = "sub2api";
  };

  services = {
    redis.servers.sub2api = {
      enable = true;
      port = 6379;
    };

    postgresql = {
      enable = true;
      ensureUsers = [
        {
          name = "sub2api";
          ensureDBOwnership = true;
        }
      ];
      ensureDatabases = [ "sub2api" ];
    };

    sub2api = {
      enable = true;
      port = 18088;
      database = {
        host = "/run/postgresql";
        user = "sub2api";
        name = "sub2api";
      };
      redis = {
        host = "127.0.0.1";
        port = 6379;
      };
      environmentFile = config.sops.secrets."sub2api/env".path;
    };
    # kanidm = {
    #   enablePam = true;
    #   clientSettings = {
    #     uri = "https://kanidm.${config.networking.domain}";
    #   };
    #   unixSettings = {
    #     default_shell = "${pkgs.zsh}/bin/zsh";
    #     home_alias = "name";
    #     home_attr = "uuid";
    #     pam_allowed_login_groups = [ "devops" ]; # Updated group to match changes in groups
    #     home_mount_prefix = "/run/kanidm:/run/kanidm";
    #   };
    # };

    nginx.virtualHosts."sub2api.xiongchenyu.dpdns.org" = {
      forceSSL = true;
      useACMEHost = "xiongchenyu.dpdns.org";
      locations."/" = {
        proxyPass = "http://127.0.0.1:18088";
        proxyWebsockets = true;
      };
    };

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
