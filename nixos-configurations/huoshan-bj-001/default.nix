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
let
  # AutoLife Robotics marketing site — SvelteKit built to static files
  # (adapter-static). The build output is the nginx document root for
  # autolife-robotics.com. Source comes from the autolife-www flake input.
  autolife-www = pkgs.buildNpmPackage {
    pname = "autolife-www";
    version = "0.0.1";
    src = inputs.autolife-www;
    npmDepsHash = "sha256-dlCY7suxYsjVVM8lqM8oJ1cITnE0OJ5ilacP8e0rKQ4=";
    installPhase = ''
      runHook preInstall
      cp -r build $out
      runHook postInstall
    '';
  };
in
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
    inputs.xiongchenyu6.nixosModules.casdoor
    protect-carrot.nixosModules.default
  ];

  # Provides pkgs.protect-carrot-web for the protect-carrot nginx module.
  nixpkgs.overlays = [
    inputs.protect-carrot.overlays.default
  ];

  zramSwap.enable = true;

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
      # The protect-carrot game loads hundreds of small assets over a single
      # HTTP/2 connection. nginx's default keepalive_requests (1000) can be
      # tripped mid-load, tearing the connection down — which bevy_asset turns
      # into a fatal panic because it unwraps on any failed fetch. Lift the cap
      # so a full asset load never recycles the connection.
      commonHttpConfig = ''
        keepalive_requests 100000;
      '';

      # AutoLife Robotics marketing site — static SvelteKit build.
      # try_files serves prerendered HTML (e.g. /about → /about.html,
      # /news/<slug> → /news/<slug>.html) and falls back to the SPA shell
      # (200.html) for any client-rendered route. brotli_static is on
      # globally (mixins-nginx); enable gzip_static for the .gz siblings.
      virtualHosts."autolife-robotics.com" = {
        forceSSL = true;
        useACMEHost = "autolife-robotics.com";
        root = autolife-www;
        locations."/" = {
          tryFiles = "$uri $uri.html $uri/index.html /200.html";
          extraConfig = ''
            gzip_static on;
          '';
        };
      };

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

    # 保卫萝卜 web game — served at parrot.bj.autolife-robotics.com.
    # *.bj.autolife-robotics.com already resolves to this host; the cert is
    # issued per-host via ACME HTTP-01 (port 80 is open in the firewall).
    protect-carrot = {
      enable = true;
      hostname = "parrot.bj.autolife-robotics.com";
      enableACME = true;
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
          environmentFile = config.sops.secrets."acme/volcengine".path;
          group = "nginx";
        };
      };
    };
  };
}
