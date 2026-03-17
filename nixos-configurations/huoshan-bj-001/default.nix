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
    ezModules.sing-box
    srvos.nixosModules.server
    ezModules.mixins-nginx
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    rust-web-server.nixosModules.rust-web-server
    ../../nixos-modules/rust-web-server-config.nix # Our local module with sops templates
    inputs.xiongchenyu6.nixosModules.casdoor
  ];

  zramSwap.enable = true;
  # rust-web-server secrets are now handled by the module itself

  boot = {
    kernelPackages = lib.mkForce pkgs.linuxPackages_latest;
    tmp.cleanOnBoot = true;
  };

  networking =
    let
      file-path = builtins.split "/" (toString ./.);
      hostName = lib.last file-path;
    in
    {
      inherit hostName;
      domain = "autolife.com";
      firewall = {
        allowedTCPPorts = [
          22
          80
          443
          2222
          7000
          8000
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
    nginx = {
      virtualHosts."casdoor.autolife-robotics.com" = {
        forceSSL = true;
        useACMEHost = "autolife-robotics.com";
        locations."/" = {
          proxyPass = "http://127.0.0.1:8000";
          proxyWebsockets = true;
        };
      };
    };

    postgresql = {
      enable = true;
      package = pkgs.postgresql_18_jit;
      authentication = ''
        local all all trust
        host  all  all 0.0.0.0/0 scram-sha-256
      '';
      enableTCPIP = true;
      ensureDatabases = [
        "rustWebServer"
        "rustwebserver"
        "casdoor"
      ];
    };

    casdoor = {
      enable = true;
      appName = "casdoor";
      port = 8000;
      runMode = "prod";
      database = {
        driver = "postgres";
        host = "localhost";
        port = 5432;
        username = "casdoor";
        # password managed by sops — injected at runtime
        name = "casdoor";
      };
      redis = {
        enable = false;
      };
      staticBaseUrl = "https://casdoor.autolife-robotics.com";
      autoStart = true;
    };

    rust-web-server = {
      enable = true;
      configFile = config.sops.templates."rust-web-server-config".path;
    };
  };

  sops.secrets."acme/volcengine" = {
    mode = "770";
    owner = "acme";
    group = "acme";
  };
  sops.secrets."casdoor/db_password" = { };

  # Inject casdoor DB password into its config at runtime
  systemd.services.casdoor.serviceConfig.ExecStartPre = lib.mkAfter [
    "+${pkgs.writeShellScript "casdoor-inject-password" ''
      cfg="/var/lib/casdoor/app.conf"
      if [ -f "$cfg" ]; then
        db_pass=$(cat ${config.sops.secrets."casdoor/db_password".path})
        ${pkgs.gnused}/bin/sed -i "s|dataSourceName = .*|dataSourceName = \"user=casdoor password=$db_pass host=localhost port=5432 dbname=casdoor sslmode=disable\"|" "$cfg"
      fi
    ''}"
  ];

  security = {
    pam.services.nginx.setEnvironment = false;

    acme = {
      acceptTerms = true;
      defaults.server = "https://acme-v02.api.letsencrypt.org/directory";
      certs = {
        "autolife-robotics.com" = {
          domain = "autolife-robotics.com";
          extraDomainNames = [ "*.autolife-robotics.com" ];
          email = "xiongchenyu6@gmail.com";
          dnsProvider = "volcengine";
          credentialsFile = config.sops.secrets."acme/volcengine".path;
          group = "nginx";
        };
      };
    };
  };
}
