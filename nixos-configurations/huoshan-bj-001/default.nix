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

    };

    # 保卫萝卜 web game — served at parrot.bj.autolife-robotics.com.
    # *.bj.autolife-robotics.com already resolves to this host; the cert is
    # issued per-host via ACME HTTP-01 (port 80 is open in the firewall).
    protect-carrot = {
      enable = true;
      hostname = "parrot.bj.autolife-robotics.com";
      enableACME = true;
      # Weak-network tolerance. The big assets (wasm ~7MB, bgm ~4.6MB) over a
      # slow/lossy mobile link can stall between writes for longer than nginx's
      # default 60s send_timeout, which then aborts the transfer and the game's
      # asset fetch fails. These are server-scoped (override the http defaults
      # for this vhost only).
      extraConfig = ''
        # Max gap between two successive writes to the client (not whole-response).
        send_timeout 300s;
        # Keep the HTTP/2 connection alive across pauses between asset bursts.
        keepalive_timeout 120s;
        # Be generous reading slow clients' request headers/body too.
        client_header_timeout 120s;
        client_body_timeout 120s;
        # Drain a closing connection slowly instead of RST-ing a lagging client.
        lingering_timeout 30s;
      '';
    };
  };

  sops.secrets."acme/volcengine" = {
    mode = "770";
    owner = "acme";
    group = "acme";
  };

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
