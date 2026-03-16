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
  mcporter = inputs.llm-agents.packages.${pkgs.system}.mcporter;

  # Workaround: nix-openclaw copies source extensions/ and compiled dist/extensions/
  # separately, but plugin manifests (openclaw.plugin.json) only exist in extensions/.
  # The gateway looks for them in dist/extensions/. Patch the package to copy them over.
  openclawPkg = pkgs.openclaw-gateway;
  patchedOpenclaw = openclawPkg.overrideAttrs (old: {
    installPhase = ''
      ${old.installPhase}
      # Fix 1: Copy plugin manifests and package.json to dist/extensions/
      for d in $out/lib/openclaw/extensions/*/; do
        name=$(basename "$d")
        if [ -d "$out/lib/openclaw/dist/extensions/$name" ]; then
          [ -f "$d/openclaw.plugin.json" ] && cp "$d/openclaw.plugin.json" "$out/lib/openclaw/dist/extensions/$name/"
          if [ -f "$d/package.json" ]; then
            ${pkgs.gnused}/bin/sed 's/\.ts"/\.js"/g' "$d/package.json" > "$out/lib/openclaw/dist/extensions/$name/package.json"
          fi
        fi
      done

      # Fix 2: Create dist/plugins/runtime/index.js stub.
      # tsdown bundles createPluginRuntime into setup-surface-*.js chunks but the
      # plugin loader resolves {packageRoot}/dist/plugins/runtime/index.js at runtime.
      # Find the chunk that exports createPluginRuntime and re-export it by name.
      mkdir -p $out/lib/openclaw/dist/plugins/runtime
      chunk=$(${pkgs.gnugrep}/bin/grep -rl 'createPluginRuntime as ' $out/lib/openclaw/dist/setup-surface-*.js | head -1)
      if [ -n "$chunk" ]; then
        alias=$(${pkgs.gnugrep}/bin/grep -oP 'createPluginRuntime as \K\w+' "$chunk" | head -1)
        chunkName=$(basename "$chunk")
        echo ">> Creating plugin runtime stub: $chunkName alias=$alias"
        cat > $out/lib/openclaw/dist/plugins/runtime/index.js <<STUB
import { $alias as createPluginRuntime } from "../../$chunkName";
export { createPluginRuntime };
STUB
      else
        echo "WARNING: Could not find createPluginRuntime export in any setup-surface chunk"
      fi
    '';
  });

  # Shared packages available to both system environment and openclaw service
  sharedToolPkgs = with pkgs; [
    nodejs_25
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
    # Import Hashtopolis server module from NUR packages
    inputs.openclaw.nixosModules.openclaw-gateway
    xiongchenyu6.nixosModules.hashtopolis-server

    ./disk-config.nix
    ./hardware-configuration.nix
    ./hashtopolis.nix # Hashtopolis server configuration

  ];

  sops.templates."openclaw-env".content = ''
    VOLCENGINE_API_KEY=${config.sops.placeholder."api-keys/VOLCENGINE_API_KEY"}
    SILICON_FLOW_API_KEY=${config.sops.placeholder."api-keys/SILICON_FLOW"}
    NVIDIA_API_KEY=${config.sops.placeholder."zeroclaw/nvidia_api_key"}
    TELEGRAM_BOT_TOKEN=${config.sops.placeholder."zeroclaw/telegram_bot_token"}
    GEMINI_API_KEY=${config.sops.placeholder."api-keys/GEMINI_API_KEY"}
  '';

  sops.templates."s3fs-passwd" = {
    content = "${config.sops.placeholder."s3fs/access_key"}:${config.sops.placeholder."s3fs/secret_key"}";
    mode = "0600";
    owner = "root";
    group = "root";
  };

  sops.secrets."s3fs/access_key" = { };
  sops.secrets."s3fs/secret_key" = { };

  sops.secrets."api-keys/GEMINI_API_KEY".owner = "root";
  sops.secrets."api-keys/SILICON_FLOW".owner = "root";
  sops.secrets."api-keys/VOLCENGINE_API_KEY".owner = "root";
  sops.secrets."zeroclaw/nvidia_api_key".owner = "openclaw";
  sops.secrets."zeroclaw/telegram_bot_token".owner = "openclaw";

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


  services.openclaw-gateway = {
    enable = true;
    package = patchedOpenclaw;
    user = "root";
    group = "root";
    createUser = false;
    port = 18789;
    servicePath =
      sharedToolPkgs
      ++ (with pkgs; [
        nix
        systemd
        sudo
        file
        wget
      ]);
    environmentFiles = [ config.sops.templates."openclaw-env".path ];

    config = {
      gateway = {
        port = 18789;
        bind = "lan";
        mode = "local";
      };
      approvals = {
        exec = {
          enabled = false;
        };
      };
      agents = {
        defaults = {
          model = {
            primary = "volcengine/ark-code-latest";
            fallbacks = [
              "google/gemini-2.5-flash"
            ];
          };
          elevatedDefault = "full";
        };
      };
      models = {
        providers = {
          volcengine = {
            baseUrl = "https://ark.cn-beijing.volces.com/api/coding/v3";
            api = "openai-completions";
            auth = "api-key";
            apiKey = "\${VOLCENGINE_API_KEY}";
            models = [
              {
                id = "ark-code-latest";
                name = "Volcengine Ark Code Latest";
                input = [ "text" ];
                contextWindow = 65536;
                maxTokens = 8192;
              }
            ];
          };
          siliconflow = {
            baseUrl = "https://api.siliconflow.cn/v1";
            api = "openai-completions";
            auth = "api-key";
            apiKey = "\${SILICON_FLOW_API_KEY}";
            models = [
              {
                id = "deepseek-ai/DeepSeek-V3";
                name = "DeepSeek V3";
                input = [ "text" ];
                contextWindow = 65536;
                maxTokens = 8192;
              }
              {
                id = "Pro/MiniMaxAI/MiniMax-M2.5";
                name = "MiniMax M2.5 (Pro)";
                input = [ "text" ];
                contextWindow = 131072;
                maxTokens = 4096;
              }
            ];
          };
          nvidia = {
            baseUrl = "https://integrate.api.nvidia.com/v1";
            api = "openai-completions";
            auth = "api-key";
            apiKey = "\${NVIDIA_API_KEY}";
            models = [
              {
                id = "minimaxai/minimax-m2.5";
                name = "MiniMax M2.5";
                input = [ "text" ];
                contextWindow = 131072;
                maxTokens = 4096;
              }
            ];
          };
        };
      };
      tools = {
        profile = "full";
        allow = [ "*" ];
        elevated = {
          enabled = true;
          allowFrom = {
            telegram = [
              "5368588092"
              "5369058954"
            ];
          };
        };
      };
      channels = {
        telegram = {
          botToken = "\${TELEGRAM_BOT_TOKEN}";
          dmPolicy = "allowlist";
          groupPolicy = "allowlist";
          execApprovals = {
            enabled = false;
            approvers = [
              "5368588092"
              "5369058954"
            ];
          };
          groups = {
            "-1003475261813" = {
              allowFrom = [
                "5368588092"
                "5369058954"
              ];
              requireMention = false;
            };
          };
          allowFrom = [
            "5368588092"
            "5369058954"
          ];
          groupAllowFrom = [
            "5368588092"
            "5369058954"
          ];
        };
      };
    };
  };

  systemd.services.openclaw-gateway.serviceConfig = {
    StandardOutput = lib.mkForce "journal";
    StandardError = lib.mkForce "journal";
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
      # Create necessary directories
      "d /var/lib/openclaw/.openclaw/workspace 0755 root root - -"
      "d /var/lib/openclaw/config 0755 root root - -"
      "d /home/freeman.xiong/config 0755 freeman.xiong users - -"

      # Symlink the generated JSON files into the configs
      "d /root/config 0755 root root - -"
      "L+ /root/config/mcporter.json - - - - ${mcporterJson}"
      "L+ /var/lib/openclaw/config/mcporter.json - - - - ${mcporterJson}"
      "L+ /home/freeman.xiong/config/mcporter.json - - - - ${mcporterJson}"
    ];

  # OpenClaw — personal AI assistant
  users.users.openclaw = {
    isSystemUser = true;
    group = "openclaw";
    home = "/var/lib/openclaw";
    createHome = true;
  };
  users.groups.openclaw = { };

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
      User = "openclaw";
      Group = "openclaw";
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
