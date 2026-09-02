# Linux-only GUI configuration: desktop apps, GTK, i18n, Qt, and Linux-specific programs
{
  inputs,
  config,
  osConfig ? null,
  pkgs,
  lib,
  ...
}:
let
  isRoot = config.home.username == "root";
  hasNvidiaTag =
    osConfig != null
    && osConfig ? system
    && osConfig.system ? nixos
    && osConfig.system.nixos ? tags
    && builtins.elem "nvidia" osConfig.system.nixos.tags;
  whisperCppPackage =
    if hasNvidiaTag then
      (pkgs.whisper-cpp.override {
        cudaSupport = true;
      }).overrideAttrs
        (old: {
          cmakeFlags = (old.cmakeFlags or [ ]) ++ [
            (lib.cmakeFeature "CMAKE_CUDA_ARCHITECTURES" "86")
          ];
        })
    else
      pkgs.whisper-cpp;
  whisperCppModel = "large-v3-turbo-q5_0";
  whisperCppModelDir = "$HOME/.local/share/whisper-cpp/models";
  agentsviewPackage = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.agentsview;
  voxtypePackage = inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.voxtype;
  voiceTermGroups = [
    {
      name = "Nix / Home Manager";
      terms = [
        "Nix"
        "NixOS"
        "nixpkgs"
        "home-manager"
        "Home Manager"
        "flake.nix"
        "flake.lock"
        "configuration.nix"
        "default.nix"
        "derivation"
        "overlay"
        "overrideAttrs"
        "mkIf"
        "mkMerge"
        "mkOption"
        "stdenv"
        "nixos-rebuild"
        "nix develop"
        "nix shell"
        "direnv"
        "Cachix"
        "NUR"
        "srvos"
        "sops-nix"
        "impermanence"
        "talon-nix"
      ];
    }
    {
      name = "Desktop / Wayland";
      terms = [
        "niri"
        "Noctalia"
        "Wayland"
        "XWayland"
        "xwayland-satellite"
        "PipeWire"
        "WirePlumber"
        "PulseAudio"
        "fcitx5"
        "Rime"
        "Rio"
        "wl-clipboard"
        "wl-copy"
        "wl-paste"
        "dotool"
        "ydotool"
        "uinput"
        "fusermount3"
        "FUSE"
        "KDE Connect"
        "Dolphin"
        "Hyprland"
        "XMonad"
        "xkb"
        "text-input-v3"
        "GTK_IM_MODULE"
        "QT_IM_MODULE"
        "NIXOS_OZONE_WL"
      ];
    }
    {
      name = "Voice Input";
      terms = [
        "Voxtype"
        "Talon"
        "whisper.cpp"
        "whisper-server"
        "ggml"
        "large-v3-turbo"
        "large-v3-turbo-q5_0"
        "OpenAI Whisper"
        "OpenAI"
        "Codex"
        "codexpro"
        "LocalAI"
        "push-to-talk"
        "transcription"
        "prompt"
        "CUDA"
        "CMAKE_CUDA_ARCHITECTURES"
        "suppress-nst"
      ];
    }
    {
      name = "Editors / AI Coding";
      terms = [
        "Cursor"
        "VS Code"
        "Zed"
        "Kiro"
        "Antigravity"
        "Copilot"
        "Claude"
        "Codex CLI"
        "OpenCode"
        "vim"
        "Neovim"
        "Tree-sitter"
        "rust-analyzer"
        "clangd"
        "Pyright"
        "Pylance"
        "nixd"
        "ESLint"
        "Prettier"
        "Markdown"
      ];
    }
    {
      name = "Languages / Build Tools";
      terms = [
        "Rust"
        "Cargo"
        "Bevy"
        "WGPU"
        "Go"
        "Python"
        "uv"
        "TypeScript"
        "JavaScript"
        "Node.js"
        "pnpm"
        "yarn"
        "Vite"
        "React"
        "Svelte"
        "Next.js"
        "CSS"
        "HTML"
        "Tailwind"
        "Haskell"
        "Scala"
        "C"
        "C++"
        "C#"
        "Zig"
        "Java"
        "Maven"
        "Gradle"
        "CMake"
        "Makefile"
        "Solidity"
        "Move"
        "Circom"
        "R1CS"
        "WebAssembly"
        "WASM"
        "GLSL"
        "SPIR-V"
      ];
    }
    {
      name = "Backend / Web";
      terms = [
        "Supabase"
        "supabase-cli"
        "PostgREST"
        "GoTrue"
        "Realtime"
        "Storage API"
        "Kong"
        "REST"
        "GraphQL"
        "gRPC"
        "WebRTC"
        "GStreamer"
        "OAuth"
        "JWT"
        "OpenAPI"
        "FastAPI"
        "Django"
        "Flask"
        "Axum"
        "Actix"
        "SQLx"
      ];
    }
    {
      name = "Infrastructure / DevOps";
      terms = [
        "Docker"
        "Podman"
        "docker-compose"
        "Compose"
        "Kubernetes"
        "Helm"
        "Terraform"
        "Ansible"
        "systemd"
        "systemd user"
        "journald"
        "journalctl"
        "nginx"
        "OpenResty"
        "Caddy"
        "Traefik"
        "Cloudflare"
        "Google Cloud SDK"
        "AWS"
        "Vault"
        "SOPS"
        "age"
        "SSH"
        "NetworkManager"
      ];
    }
    {
      name = "Database / Observability";
      terms = [
        "PostgreSQL"
        "MySQL"
        "Redis"
        "SQLite"
        "MongoDB"
        "Elasticsearch"
        "ClickHouse"
        "Grafana"
        "Prometheus"
        "Loki"
        "Promtail"
        "Parseable"
        "Datadog"
        "node-exporter"
        "healthcheck"
        "S3"
        "MinIO"
        "RabbitMQ"
        "Kafka"
        "MQTT"
      ];
    }
    {
      name = "Common File Names";
      terms = [
        "package.json"
        "Cargo.toml"
        "go.mod"
        "pyproject.toml"
        "docker-compose.yml"
        "compose.yml"
        ".env"
        "README.md"
        "TODO"
        "localhost"
        "127.0.0.1"
      ];
    }
  ];
  voicePromptTerms = lib.unique (lib.concatMap (group: lib.take 14 group.terms) voiceTermGroups);
  privateVoiceTermsPath = if isRoot then "/dev/null" else config.sops.secrets."voxtype/terms".path;
  voicePrompt = "中英混合编程口述。保留英文项目名、命令名、文件名和技术术语。常见公开术语包括: ${lib.concatStringsSep " " voicePromptTerms}.";
  voicePromptRuntime = ''
    build_voice_prompt() {
      prompt=${lib.escapeShellArg voicePrompt}
      private_terms=""
      private_terms_count=0

      if [ -r "${privateVoiceTermsPath}" ]; then
        while IFS= read -r line; do
          if [ "$private_terms_count" -ge 120 ]; then
            break
          fi
          case "$line" in
            ""|\#*) continue ;;
          esac
          private_terms="$private_terms $line"
          private_terms_count=$((private_terms_count + 1))
        done < "${privateVoiceTermsPath}"
      fi

      if [ -n "$private_terms" ]; then
        prompt="$prompt 私有项目和本地术语包括:$private_terms."
      fi

      printf '%s' "$prompt"
    }
  '';
  voxtypeDaemon = pkgs.writeShellScript "voxtype-daemon" ''
    set -euo pipefail

    ${voicePromptRuntime}

    exec ${voxtypePackage}/bin/voxtype \
      --initial-prompt "$(build_voice_prompt)" \
      -q daemon
  '';
  whisperCppServer = pkgs.writeShellScript "whisper-cpp-server-local" ''
    set -euo pipefail

    export PATH=${
      lib.makeBinPath [
        pkgs.coreutils
        pkgs.curl
        pkgs.ffmpeg-full
        whisperCppPackage
      ]
    }:$PATH

    mkdir -p "${whisperCppModelDir}"
    whisper-cpp-download-ggml-model ${whisperCppModel} "${whisperCppModelDir}"

    exec whisper-server \
      --model "${whisperCppModelDir}/ggml-${whisperCppModel}.bin" \
      --host 127.0.0.1 \
      --port 8080 \
      --inference-path /v1/audio/transcriptions \
      --convert \
      --language auto \
      --suppress-nst \
      --prompt ${lib.escapeShellArg voicePrompt}
  '';
in
{
  qt = {
    enable = true;
  };

  home = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
    pointerCursor = {
      enable = true;
      name = "Vanilla-DMZ";
      package = pkgs.vanilla-dmz;
      x11 = {
        enable = true;
      };
      size = 32;
    };
    # Fix for winetricks wine64 detection issue
    file.".local/bin/wine64" = {
      source = "${pkgs.wineWow64Packages.waylandFull}/bin/wine";
      executable = true;
    };
    file.".local/bin/warcraft3" = {
      source = ./warcraft3.sh;
      executable = true;
    };
    file.".config/voxtype/config.toml".text = ''
      state_file = "auto"

      [hotkey]
      enabled = false
      mode = "toggle"

      [audio]
      device = "default"
      sample_rate = 16000
      max_duration_secs = 60

      [whisper]
      mode = "remote"
      language = "auto"
      translate = false
      remote_endpoint = "http://127.0.0.1:8080"
      remote_model = "whisper-1"
      remote_timeout_secs = 60

      [output]
      mode = "type"
      fallback_to_clipboard = true
      shift_enter_newlines = true
      driver_order = ["wtype", "dotool", "ydotool", "clipboard"]

      [output.notification]
      on_recording_start = true
      on_recording_stop = true
      on_transcription = true
    '';
    packages =
      (with pkgs; [
        # Linux-only GUI/desktop
        aspell
        kiro
        aspellDicts.en
        supabase-cli
        camber
        baidupcs-go
        sbctl
        #nur.repos.xddxdd.qq
        nur.repos.xddxdd.bilibili
        #nur.repos.xddxdd.dingtalk
        #gimp
        #gitkraken
        # netbird-ui
        #termius
        unrar-wrapper
        moonlight-qt # Sunshine client — stream from sg-office or any Sunshine host
        #whatsapp-for-linux
        karere
        winetricks
        wineWow64Packages.waylandFull
        kdePackages.dolphin
        kdePackages.qtwayland
        kdePackages.qt6ct
        # kdePackages.wayqt
        # kdePackages.qtstyleplugin-kvantum
        feishu-lark
        # 原生 Wayland 启动:XWayland 下 wtype(voxtype 语音输入)打中文会因
        # xwayland-satellite 不转发临时 keymap 而变成数字;enable-wayland-ime
        # 让 fcitx5 走 text-input-v3
        (feishu.override {
          commandLineArgs = "--ozone-platform-hint=auto --enable-wayland-ime --wayland-text-input-version=3";
        })
        android-studio
        #opengl-driver
        libsecret
        vulkan-loader
        vulnix
        nix-melt
        blender
        #microsoft-edge
        gotron-sdk
        #vsc-leetcode-cli
        my2sql
        # nix-du
        #pg-ldap-sync
        rustscan
        #stow
        ugm
        rkdeveloptool
        # terraform
        # terraform-ls
        # terracognita
        # terranix
        # terraformer
        # tf2pulumi
        #localstack
        desktop-file-utils
        #inputs.claude-desktop.packages.${system}.claude-desktop-with-fhs
        gnome-software
        gws
        google-cloud-sdk
        tradingview

        # Voice coding and dictation
        pavucontrol
        voxtypePackage
        whisperCppPackage

        # 成像/磁盘工具
        ddrescue
        smartmontools
        hdparm
        util-linux
        coreutils

        # NTFS 只读挂载（可选但建议）
        ntfs3g

        # 取证（命令行）
        sleuthkit

        # 恢复/雕刻（PhotoRec 在 testdisk 包里）
        testdisk
        autopsy
      ])
      ++ lib.optionals pkgs.stdenv.hostPlatform.isLinux (
        with pkgs;
        [
          godot_4
        ]
      );
    sessionVariables = {
      STARSHIP_LOG = "error";
      NIXPKGS_ALLOW_UNFREE = 1;
    };
    sessionPath = [ "$HOME/.local/bin" ];
  };

  systemd.user.services.whisper-cpp-server = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
    Unit = {
      Description = "Local whisper.cpp OpenAI-compatible transcription server";
      After = [ "pipewire.service" ];
    };

    Service = {
      ExecStart = "${whisperCppServer}";
      Restart = "on-failure";
      RestartSec = 5;
    };

    Install = {
      WantedBy = [ "default.target" ];
    };
  };

  systemd.user.services.agentsview = lib.mkIf (pkgs.stdenv.hostPlatform.isLinux && !isRoot) {
    Unit.Description = "Local AI coding agent session viewer";

    Service = {
      ExecStart = "${agentsviewPackage}/bin/agentsview serve --host 127.0.0.1 --port 8788 --no-browser --no-update-check";
      Restart = "on-failure";
      RestartSec = 5;
    };

    Install.WantedBy = [ "default.target" ];
  };

  # Split out of common.yaml: the term list is prose, and at ~2.5kB it was a
  # quarter of that file's plaintext, so every unrelated secret edit churned it.
  sops.secrets."voxtype/terms" = lib.mkIf (pkgs.stdenv.hostPlatform.isLinux && !isRoot) {
    sopsFile = ../../secrets/voxtype-terms.yaml;
  };

  systemd.user.services.voxtype = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
    Unit = {
      Description = "Voxtype push-to-talk voice-to-text daemon";
      After = [
        "graphical-session.target"
        "pipewire.service"
        "whisper-cpp-server.service"
      ];
      PartOf = [ "graphical-session.target" ];
      Wants = [ "whisper-cpp-server.service" ];
    };

    Service = {
      ExecStart = "${voxtypeDaemon}";
      Restart = "on-failure";
      RestartSec = 2;
    };

    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };

  gtk = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
    enable = true;
    gtk4 = {
      extraConfig = {
        gtk-cursor-blink = false;
        gtk-recent-files-limit = 20;
      };
    };
  };

  i18n = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
    inputMethod = {
      type = "fcitx5";
      enable = true;
      fcitx5 = {
        # systemd.enable defaults to true upstream and ships
        # fcitx5-daemon.service, which is the single owner of the fcitx5
        # process. The XDG autostart entry the fcitx5 package installs into
        # the profile is suppressed below so the two don't race.
        addons = with pkgs; [
          #fcitx5-mozc
          fcitx5-gtk
          #fcitx5-chinese-addons
          # 方案数据由 rime-frost 包提供,用户目录只放 *.custom.yaml 定制
          (fcitx5-rime.override { rimeDataPkgs = [ rime-frost ]; })
        ];
      };
    };
  };

  # systemd-xdg-autostart-generator turns the profile's
  # etc/xdg/autostart/org.fcitx.Fcitx5.desktop into a second unit under
  # xdg-desktop-autostart.target, which niri-session activates. A user-level
  # entry of the same name wins per the XDG autostart spec, so Hidden=true
  # removes it and leaves fcitx5-daemon.service alone.
  xdg.configFile = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
    "autostart/org.fcitx.Fcitx5.desktop".text = ''
      [Desktop Entry]
      Type=Application
      Name=Fcitx 5
      Exec=fcitx5
      Hidden=true
    '';
  };
  programs = {
    vastai = {
      enable = true;

      sshConfig = {
        enable = false;
        # Optional: specify API key file path
        # apiKeyFile = /path/to/api/key;
        # Defaults to ~/.config/vastai/vast_api_key
      };
    };

    nix-init = {
      enable = true;
    };

    zathura = {
      enable = true;
    };

    texlive = {
      enable = false;
      extraPackages = tpkgs: {
        inherit (tpkgs)
          collection-basic
          collection-luatex
          collection-langcjk
          collection-latexrecommended
          collection-fontsrecommended
          collection-xetex
          latexmk
          appendix
          biber
          awesomebox
          fontawesome5
          changepage
          csquotes
          algorithms
          algorithmicx
          algpseudocodex
          titlesec
          fontspec
          microtype
          amsmath
          amssymb
          mathtools
          xfrac
          ;
      };
    };

    bun = {
      enable = true;
    };
    # carapace.enable = true;
    # comodoro.enable = true;
    mpv.enable = true;

    obs-studio = lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
      enable = true;
      plugins = with pkgs.obs-studio-plugins; [
        wlrobs
        obs-pipewire-audio-capture
      ];
    };
    # thunderbird = {
    #   enable = true;
    #   profiles = {
    #     "xiongchenyu6@gmail.com" = {
    #       isDefault = true;
    #       withExternalGnupg = true;
    #     };
    #   };
    # };

    obsidian = {
      enable = true;
    };

    zed-editor = {
      enable = true;
      extensions = [ "nix" ];
    };

    # password-store = {
    #   enable = true;
    # };
  };
}
