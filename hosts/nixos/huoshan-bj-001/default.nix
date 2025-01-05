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
    # ../../../profiles/common/components/datadog-agent.nix
    ../../../profiles/cn.nix
  ];
  sops.secrets."netbird/coturn/password" = {
    owner = "turnserver";
    group = "turnserver";
  };
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
        "vr-sg.autolife-robotics.me" = {
          locations = {
            "/" = {
              proxyPass = "http://localhost:8080";
            };
          };
        };
        "frp-dashboard.autolife-robotics.me" = {
          locations = {
            "/" = {
              proxyPass = "http://localhost:7500";
            };
          };
        };
        "mngt.autolife-robotics.me" = {
          locations = {
            "/" = {
              proxyWebsockets = true;
              proxyPass = "http://localhost:5555";
            };
          };
        };
        # "autolife.ai" = {
        #   addSSL = true;
        #   acmeRoot = null;
        #   useACMEHost = "ai";
        #   kTLS = true;
        #   locations = {
        #     "/" = with pkgs; {
        #       root = www_dist;
        #     };
        #   };
        # };
        # "www.autolife.ai" = {
        #   addSSL = true;
        #   acmeRoot = null;
        #   useACMEHost = "ai";
        #   kTLS = true;
        #   locations = {
        #     "/" = with pkgs; {
        #       root = www_dist;
        #     };
        #   };
        # };

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
