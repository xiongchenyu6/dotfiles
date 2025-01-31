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
    ezModules.sops
    ezModules.cn
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    vscode-server.nixosModules.default
  ];

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
        enable = true;
        allowedTCPPorts = [
          22
          80
          443
          2222
          5432
          7000
        ];
        allowedUDPPorts = [
          89
          179
          2222
          3478
          7000
          7777
          6696
          33434
        ];
      };
    };

  services = {
    robotSignalDashboard = {
      enable = true;
      configFile = ./config.json;
    };

    coturn = {
      enable = true;
    };

    frp = {
      enable = true;
      role = "server";
      settings = {
        bindPort = 7000;
        bindAddr = "0.0.0.0";
        kcpBindPort = 7000;
        vhostHTTPPort = 8080;
        webserver = {
          port = 7500;
          user = "admin";
          password = "admin";
        };
        auth = {
          method = "token";
          token = builtins.getEnv "FRP";
        };
      };
    };

    nginx = {
      virtualHosts = {
        "weaw.eheja.cn" = {
          forceSSL = true;
          sslCertificateKey = ./weaw.eheja.cn.key;
          sslCertificate = ./weaw.eheja.cn.crt;
          locations = {
            "/" = {
              proxyPass = "http://localhost:8080";
            };
          };
        };

        "weaw.eheja.cn:8888" = {
          serverName = "weaw.eheja.cn";
          forceSSL = true;
          sslCertificateKey = ./weaw.eheja.cn.key;
          sslCertificate = ./weaw.eheja.cn.crt;
          listen = [
            {
              addr = "0.0.0.0";
              port = 8888;
              ssl = true;
            }
          ];
          locations = {
            "/" = {
              root = "/var/www/dist";
            };
          };
        };

        "weaw.eheja.cn:5556" = {
          forceSSL = true;
          sslCertificateKey = ./weaw.eheja.cn.key;
          sslCertificate = ./weaw.eheja.cn.crt;
          serverName = "weaw.eheja.cn";
          listen = [
            {
              addr = "0.0.0.0";
              port = 5556;
              ssl = true;
            }
          ];
          locations = {
            "/" = {
              proxyPass = "http://localhost:5555";
              proxyWebsockets = true;
            };
          };
        };
      };
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
