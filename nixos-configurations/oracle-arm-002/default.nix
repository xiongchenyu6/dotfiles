{
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
    srvos.nixosModules.mixins-nginx
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
    # Import Hashtopolis server module from NUR packages
    xiongchenyu6.nixosModules.hashtopolis-server
    ./disk-config.nix
    ./hardware-configuration.nix
    ./hashtopolis.nix # Hashtopolis server configuration

  ];
  boot = {
    loader.grub = {
      # no need to set devices, disko will add all devices that have a EF02 partition to the list already
      # devices = [ ];
      efiSupport = true;
      efiInstallAsRemovable = true;
    };
    kernel = {
      sysctl = {
        "net.ipv4.ip_forward" = 1;
      };
    };
  };

  users = {
    users = {
      root = {
        openssh = {
          authorizedKeys.keys = [
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINCXyKpDU97Js6YcRIL+8M/vOdPwRKlMpTzmjGLJmCHT rachelove24@Rachels-MacBook-Air-2.local"
          ];
        };
      };
    };
  };

  environment.systemPackages =
    map lib.lowPrio [
      pkgs.curl
      pkgs.gitMinimal
    ]
    ++ (with pkgs; [
      nodejs_22
    ]);

  nixpkgs = {
    hostPlatform = "aarch64-linux";
  };

  # OpenClaw — personal AI assistant
  users.users.openclaw = {
    isSystemUser = true;
    group = "openclaw";
    home = "/var/lib/openclaw";
    createHome = true;
  };
  users.groups.openclaw = { };

  systemd.services.openclaw = {
    description = "OpenClaw Gateway Service";
    after = [
      "network-online.target"
      "sops-nix.service"
    ];
    wants = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];

    path = with pkgs; [
      nodejs_22
      git
      bash
      coreutils
      gnused
      gnutar
      gzip
      findutils
      gnugrep
    ];

    # Install openclaw globally on first start or update
    preStart = ''
      export HOME=/var/lib/openclaw
      export NPM_CONFIG_PREFIX=/var/lib/openclaw/.npm-global
      export PATH="$NPM_CONFIG_PREFIX/bin:$PATH"
      if [ ! -x "$NPM_CONFIG_PREFIX/bin/openclaw" ]; then
        echo "Installing openclaw..."
        ${pkgs.nodejs_22}/bin/npm install -g openclaw@latest --prefix "$NPM_CONFIG_PREFIX" --ignore-scripts
      fi
    '';

    script = ''
      export HOME=/var/lib/openclaw
      export NPM_CONFIG_PREFIX=/var/lib/openclaw/.npm-global
      export PATH="$NPM_CONFIG_PREFIX/bin:$PATH"
      # Create minimal config if none exists
      mkdir -p /var/lib/openclaw/.openclaw
      if [ ! -f /var/lib/openclaw/.openclaw/openclaw.json ]; then
        cat > /var/lib/openclaw/.openclaw/openclaw.json << 'CONF'
      {
        "gateway": {
          "port": 18789,
          "bind": "lan",
          "mode": "local"
        }
      }
      CONF
      fi
      exec openclaw gateway --port 18789
    '';

    serviceConfig = {
      User = "openclaw";
      Group = "openclaw";
      WorkingDirectory = "/var/lib/openclaw";
      StateDirectory = "openclaw";
      Restart = "always";
      RestartSec = 5;
      # Hardening
      ProtectHome = "read-only";
      PrivateTmp = true;
      NoNewPrivileges = true;
    };
  };
}
