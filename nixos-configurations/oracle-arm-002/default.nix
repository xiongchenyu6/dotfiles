{
  inputs,
  modulesPath,
  lib,
  config,
  pkgs,
  ezModules,
  ...
}:
let
  mcporter = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.mcporter;

  # Build zeroclaw from source (Rust binary)
  zeroclaw = pkgs.rustPlatform.buildRustPackage {
    pname = "zeroclaw";
    version = inputs.zeroclaw.shortRev or "unstable";
    src = inputs.zeroclaw;

    cargoLock = {
      lockFile = "${inputs.zeroclaw}/Cargo.lock";
      allowBuiltinFetchGit = true;
    };

    nativeBuildInputs = with pkgs; [
      pkg-config
      cmake
    ];

    buildInputs = with pkgs; [
      openssl
      sqlite
    ];

    buildNoDefaultFeatures = true;
    buildFeatures = [
      "sandbox-landlock"
      "skill-creation"
    ];

    # Only build the main zeroclaw binary (skip tauri app and robot-kit)
    cargoBuildFlags = [ "-p" "zeroclawlabs" ];

    doCheck = false;

    meta = {
      description = "Fast, small, and fully autonomous AI personal assistant infrastructure";
      homepage = "https://github.com/zeroclaw-labs/zeroclaw";
      license = with lib.licenses; [ mit asl20 ];
      mainProgram = "zeroclaw";
    };
  };

  # Generate zeroclaw TOML config from Nix attrset
  zeroclawConfigToml = (pkgs.formats.toml { }).generate "config.toml" {
    default_provider = "volcengine";

    gateway = {
      port = 18789;
      bind = "lan";
    };

    approvals.exec.enabled = false;

    agents.defaults = {
      elevated_default = "full";
      model = {
        primary = "volcengine/ark-code-latest";
        fallbacks = [ "google/gemini-2.5-flash" ];
      };
    };

    models.providers = {
      volcengine = {
        base_url = "https://ark.cn-beijing.volces.com/api/coding/v3";
        api = "openai-completions";
        auth = "api-key";
        api_key = "\${VOLCENGINE_API_KEY}";
        models = [
          {
            id = "ark-code-latest";
            name = "Volcengine Ark Code Latest";
            input = [ "text" ];
            context_window = 65536;
            max_tokens = 8192;
          }
        ];
      };
      siliconflow = {
        base_url = "https://api.siliconflow.cn/v1";
        api = "openai-completions";
        auth = "api-key";
        api_key = "\${SILICON_FLOW_API_KEY}";
        models = [
          {
            id = "deepseek-ai/DeepSeek-V3";
            name = "DeepSeek V3";
            input = [ "text" ];
            context_window = 65536;
            max_tokens = 8192;
          }
          {
            id = "Pro/MiniMaxAI/MiniMax-M2.5";
            name = "MiniMax M2.5 (Pro)";
            input = [ "text" ];
            context_window = 131072;
            max_tokens = 4096;
          }
        ];
      };
      nvidia = {
        base_url = "https://integrate.api.nvidia.com/v1";
        api = "openai-completions";
        auth = "api-key";
        api_key = "\${NVIDIA_API_KEY}";
        models = [
          {
            id = "minimaxai/minimax-m2.5";
            name = "MiniMax M2.5";
            input = [ "text" ];
            context_window = 131072;
            max_tokens = 4096;
          }
        ];
      };
    };

    tools = {
      profile = "full";
      allow = [ "*" ];
      elevated = {
        enabled = true;
        allow_from.telegram = [
          "5368588092"
          "5369058954"
        ];
      };
    };

    channels.telegram = {
      bot_token = "\${TELEGRAM_BOT_TOKEN}";
      dm_policy = "allowlist";
      group_policy = "allowlist";
      allow_from = [
        "5368588092"
        "5369058954"
      ];
      group_allow_from = [
        "5368588092"
        "5369058954"
      ];
      exec_approvals = {
        enabled = false;
        approvers = [
          "5368588092"
          "5369058954"
        ];
      };
      groups."-1003475261813" = {
        allow_from = [
          "5368588092"
          "5369058954"
        ];
        require_mention = false;
      };
    };
  };

  # Shared packages available to both system environment and zeroclaw service
  sharedToolPkgs = with pkgs; [
    bash
    coreutils
    gnused
    gnutar
    gzip
    findutils
    gnugrep
    git
    scrot
    imagemagick
    mcporter
  ];
in
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
    xiongchenyu6.nixosModules.hashtopolis-server

    ./disk-config.nix
    ./hardware-configuration.nix
    ./hashtopolis.nix # Hashtopolis server configuration

  ];

  sops.templates."zeroclaw-env".content = ''
    VOLCENGINE_API_KEY=${config.sops.placeholder."api-keys/VOLCENGINE_API_KEY"}
    SILICON_FLOW_API_KEY=${config.sops.placeholder."api-keys/SILICON_FLOW"}
    NVIDIA_API_KEY=${config.sops.placeholder."zeroclaw/nvidia_api_key"}
    TELEGRAM_BOT_TOKEN=${config.sops.placeholder."zeroclaw/telegram_bot_token"}
    GEMINI_API_KEY=${config.sops.placeholder."api-keys/GEMINI_API_KEY"}
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
  sops.secrets."api-keys/SILICON_FLOW".owner = "root";
  sops.secrets."api-keys/VOLCENGINE_API_KEY".owner = "root";
  sops.secrets."zeroclaw/nvidia_api_key".owner = "zeroclaw";
  sops.secrets."zeroclaw/telegram_bot_token".owner = "zeroclaw";

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
    ++ sharedToolPkgs
    ++ [
      zeroclaw
    ]
    ++ (with pkgs; [
      xvfb-run
      x11vnc
      novnc
      chromium
      xorg-server
      s3fs
    ]);

  nixpkgs = {
    hostPlatform = "aarch64-linux";
  };

  networking.firewall.allowedTCPPorts = [
    6080
  ];

  # ZeroClaw — personal AI assistant (Rust rewrite of OpenClaw)
  # One-shot migration service: runs `zeroclaw migrate openclaw` on first activation
  # to carry over memory, skills, and workspace from ~/.openclaw/ to ~/.zeroclaw/
  systemd.services.zeroclaw-migrate = {
    description = "Migrate OpenClaw data to ZeroClaw";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    before = [ "zeroclaw.service" ];
    wantedBy = [ "multi-user.target" ];

    # Only run if the old openclaw data directory exists and zeroclaw hasn't been migrated yet
    unitConfig.ConditionPathExists = "/var/lib/zeroclaw/.openclaw";

    serviceConfig = {
      Type = "oneshot";
      User = "zeroclaw";
      Group = "zeroclaw";
      WorkingDirectory = "/var/lib/zeroclaw";
      ExecStart = "${zeroclaw}/bin/zeroclaw migrate openclaw";
      RemainAfterExit = true;
      EnvironmentFile = config.sops.templates."zeroclaw-env".path;
    };
  };

  # Main zeroclaw daemon service
  systemd.services.zeroclaw = {
    description = "ZeroClaw AI assistant daemon";
    after = [
      "network-online.target"
      "zeroclaw-migrate.service"
    ];
    wants = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];

    environment = {
      HOME = "/var/lib/zeroclaw";
      ZEROCLAW_CONFIG = toString zeroclawConfigToml;
    };

    path =
      sharedToolPkgs
      ++ (with pkgs; [
        nix
        systemd
        sudo
        file
        wget
      ]);

    serviceConfig = {
      Type = "simple";
      User = "root";
      Group = "root";
      WorkingDirectory = "/var/lib/zeroclaw";
      ExecStart = "${zeroclaw}/bin/zeroclaw daemon";
      Restart = "on-failure";
      RestartSec = 10;
      EnvironmentFile = config.sops.templates."zeroclaw-env".path;
      StandardOutput = "journal";
      StandardError = "journal";
    };
  };

  systemd.tmpfiles.rules =
    let
      mcporterJson = pkgs.writeText "mcporter.json" (
        builtins.toJSON {
          mcpServers = { };
          imports = [ ];
        }
      );
    in
    [
      # Create necessary directories for zeroclaw
      "d /var/lib/zeroclaw/.zeroclaw/workspace 0755 zeroclaw zeroclaw - -"
      "d /var/lib/zeroclaw/config 0755 zeroclaw zeroclaw - -"
      "d /home/freeman.xiong/config 0755 freeman.xiong users - -"

      # Copy old openclaw data directory if it exists (for migration)
      "C /var/lib/zeroclaw/.openclaw 0755 zeroclaw zeroclaw - /var/lib/openclaw/.openclaw"

      # Symlink the generated JSON files into the configs
      "d /root/config 0755 root root - -"
      "L+ /root/config/mcporter.json - - - - ${mcporterJson}"
      "L+ /var/lib/zeroclaw/config/mcporter.json - - - - ${mcporterJson}"
      "L+ /home/freeman.xiong/config/mcporter.json - - - - ${mcporterJson}"

      # Symlink zeroclaw config
      "L+ /var/lib/zeroclaw/.zeroclaw/config.toml - - - - ${zeroclawConfigToml}"
    ];

  # ZeroClaw system user (replaces openclaw user)
  users.users.zeroclaw = {
    isSystemUser = true;
    group = "zeroclaw";
    home = "/var/lib/zeroclaw";
    createHome = true;
  };
  users.groups.zeroclaw = { };

  # Virtual display support for browser automation
  systemd.services.xvfb = {
    description = "X virtual framebuffer for headless display";
    after = [ "network-online.target" ];
    wants = [ "network-online.target" ];
    wantedBy = [ "multi-user.target" ];

    script = ''
      exec ${pkgs.xorg-server}/bin/Xvfb :99 -screen 0 1280x1024x24 -ac +extension GLX +render -noreset
    '';

    serviceConfig = {
      User = "zeroclaw";
      Group = "zeroclaw";
      Restart = "always";
      RestartSec = 5;
      Type = "simple";
    };
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
