{
  inputs,
  modulesPath,
  lib,
  config,
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
    ezModules.mixins-nginx
    srvos.nixosModules.mixins-trusted-nix-caches
    srvos.nixosModules.mixins-nix-experimental
    srvos.nixosModules.mixins-tracing
    # xiongchenyu6.nixosModules.hashtopolis-server # TEMP: disabled — composerVendorCheckHook fails after nixpkgs bump; re-enable once composer.lock is fixed upstream
    hermes-agent.nixosModules.default

    ./disk-config.nix
    ./hardware-configuration.nix
    # ./hashtopolis.nix # TEMP: disabled alongside hashtopolis-server module
  ];

  # Secret env file consumed by hermes-agent. Stored under the legacy
  # `zeroclaw/telegram_bot_token` SOPS key — the YAML namespace in
  # secrets/common.yaml is unchanged; only the nix references migrated.
  sops.templates."hermes-env".content = ''
    GEMINI_API_KEY=${config.sops.placeholder."api-keys/GEMINI_API_KEY"}
    TELEGRAM_BOT_TOKEN=${config.sops.placeholder."zeroclaw/telegram_bot_token"}
  '';

  sops.templates."s3fs-passwd" = {
    content = "${config.sops.placeholder."s3fs/access_key"}:${
      config.sops.placeholder."s3fs/secret_key"
    }";
    mode = "0600";
    owner = "root";
    group = "root";
  };

  sops.secrets."s3fs/access_key" = { };
  sops.secrets."s3fs/secret_key" = { };

  sops.secrets."api-keys/GEMINI_API_KEY".owner = "root";
  sops.secrets."zeroclaw/telegram_bot_token".owner = "root";

  boot = {
    loader.grub = {
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
    ++ [ pkgs.s3fs ];

  nixpkgs = {
    hostPlatform = "aarch64-linux";
  };

  services.hermes-agent = {
    enable = true;
    settings = {
      model = {
        default = "google/gemini-2.5-flash";
        provider = "gemini";
      };
    };
    # Non-secret env vars (bot allowlist). Secrets go via environmentFiles.
    environment = {
      TELEGRAM_ALLOWED_USERS = "5368588092,5369058954";
    };
    environmentFiles = [ config.sops.templates."hermes-env".path ];
  };

  # S3 FUSE mount for iDrive e2 docs bucket
  fileSystems."/mnt/s3/docs" = {
    device = "docs";
    fsType = "fuse./run/current-system/sw/bin/s3fs";
    noCheck = true;
    options = [
      "_netdev"
      "allow_other"
      "use_path_request_style"
      "url=https://s3.us-west-1.idrivee2.com"
      "endpoint=us-west-1"
      "passwd_file=${config.sops.templates."s3fs-passwd".path}"
    ];
  };
}
