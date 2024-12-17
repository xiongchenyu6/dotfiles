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
  users.users.root = {
    openssh.authorizedKeys.keys = lib.mkDefault [
      profiles.share.users-dict."freeman.xiong".public-key
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDc+zRGg+dTUqo0XAbCGuyGZiRlZwY0MdjOCrpZpcNDBAIk940/epjlkKh/2WTz4hC4hkv4Ms30cCbcbAByATNDXyVKwUrxkAJnFsXU4dgrYZJ0WR+67AeKRb+41daEFhmSQQztji5KmyF0uCMkBGNPC3jF63ybPj0UAS59g761t25P21IeS6zOf5IjBDIxp7JvtgOnIoOT4qQDTJijdCqjJJ/vP8bWmvS8mCMS5HDAKHgfEk5a3eJGFR8AngFSp1DkH5Q9y+YkM42IVrU2UkT8a4Qi/J2BCUKUCDBSeEmmgoBd8NOpkwvjcm6HY92sVZLSjeIyuZrEy9luNMC38PZ1SLNhDDiESIYiFpBWDKrOH8TkZStwpwwIb6hcm/+Tew5kYQlyAeCu2ZT97TRp7978cJP9Isz2kSdLRiLp57T4462feYEOvEJrBxultnCdYk6h/B9KJ81XDGG6Zt9i4A6jUBA26a7TEA6RAr8YLRKzUtr5BHpkYcAfVYTysDklU00= summer@summers-MacBook-Pro.local"
    ];
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
