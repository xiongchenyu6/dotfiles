{
  profiles,
  config,
  lib,
  pkgs,
  modulesPath,
  ...
}:
{

  zramSwap.enable = true;

  imports = [
    ./hardware-configuration.nix
    ../../../users/root/nixos.nix
    ../../../users/freeman.xiong
    ../../../profiles/sops.nix
    ../../../profiles/core/nixos.nix
    ../../../profiles/server/components
    ../../../profiles/common/components
    ../../../profiles/common/components/datadog-agent.nix
    ../../../profiles/cn.nix
  ];
  # sops.secrets."netbird/coturn/password" = {
  #   owner = "turnserver";
  #   group = "turnserver";
  # };
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
          80
          443
          5556
          7000
          8888
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
          token = builtins.readFile ../../../secrets/frp.token;
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
        imports = [
          ../../../users/profiles/cli/shell/zsh/common.nix
          ../../../users/profiles/cli/common.nix
          ../../../users/profiles/cli/tmux.nix
        ];
      };
    };
  };
}
