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
      }).overrideAttrs (old: {
        cmakeFlags = (old.cmakeFlags or [ ]) ++ [
          (lib.cmakeFeature "CMAKE_CUDA_ARCHITECTURES" "86")
        ];
      })
    else
      pkgs.whisper-cpp;
  whisperCppModel = "large-v3-turbo-q5_0";
  whisperCppModelDir = "$HOME/.local/share/whisper-cpp/models";
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
        "Ghostty"
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
        "VoxInput"
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
  voiceTerms = lib.unique (lib.concatMap (group: group.terms) voiceTermGroups);
  voicePromptTerms = lib.unique (lib.concatMap (group: lib.take 14 group.terms) voiceTermGroups);
  privateVoiceTermsPath =
    if isRoot then
      "/dev/null"
    else
      config.sops.secrets."voxinput/terms".path;
  voiceTermsText =
    lib.concatStringsSep "\n" (
      lib.concatMap (group: [ "# ${group.name}" ] ++ group.terms ++ [ "" ]) voiceTermGroups
    )
    + "\n";
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
  voxinputPasteDotool = pkgs.writeShellScriptBin "dotool" ''
    set -euo pipefail

    export YDOTOOL_SOCKET="''${YDOTOOL_SOCKET:-/run/ydotoold/socket}"

    log() {
      printf 'voxinput-dotool: %s\n' "$*" >&2
    }

    paste_text() {
      if ! ${pkgs.wl-clipboard}/bin/wl-copy --type text/plain; then
        log "wl-copy failed"
        return 1
      fi
      ${pkgs.coreutils}/bin/sleep 0.08

      if printf 'key ctrl+shift+v\n' | ${pkgs.dotool}/bin/dotool; then
        return 0
      fi

      log "dotool paste shortcut failed, trying ydotool"
      if ${pkgs.ydotool}/bin/ydotool key 29:1 42:1 47:1 47:0 42:0 29:0; then
        return 0
      fi

      log "ydotool paste shortcut failed"
      return 1
    }

    if [ "$#" -gt 0 ]; then
      if [ "$1" = "type" ]; then
        shift
        printf '%s' "$*" | paste_text
        exit 0
      fi
      exec ${pkgs.dotool}/bin/dotool "$@"
    fi

    input="$(${pkgs.coreutils}/bin/cat)"
    first_line="''${input%%$'\n'*}"
    rest="''${input#"$first_line"}"
    if [[ "$first_line" == type\ * && ( -z "$rest" || "$rest" == $'\n' ) ]]; then
      text="''${first_line#type }"
      printf '%s' "$text" | paste_text
      exit 0
    fi

    log "falling back to original dotool for non-type command batch"
    printf '%s\n' "$input" | ${pkgs.dotool}/bin/dotool
  '';
  voxinputClipboard = pkgs.voxinput.overrideAttrs (old: {
    postFixup = (old.postFixup or "") + ''
      rm -f $out/bin/voxinput
      makeWrapper $out/bin/.voxinput-wrapped $out/bin/voxinput \
        --prefix PATH : ${lib.makeBinPath [
          voxinputPasteDotool
          pkgs.wl-clipboard
          pkgs.ydotool
          pkgs.coreutils
        ]}
    '';
  });
  voxinputListener = pkgs.writeShellScript "voxinput-listener" ''
    set -euo pipefail

    ${voicePromptRuntime}

    export VOXINPUT_PROMPT="$(build_voice_prompt)"
    exec ${voxinputClipboard}/bin/voxinput listen --no-realtime
  '';
  whisperCppServer = pkgs.writeShellScript "whisper-cpp-server-local" ''
    set -euo pipefail

    export PATH=${lib.makeBinPath [
      pkgs.coreutils
      pkgs.curl
      pkgs.ffmpeg-full
      whisperCppPackage
    ]}:$PATH

    mkdir -p "${whisperCppModelDir}"
    whisper-cpp-download-ggml-model ${whisperCppModel} "${whisperCppModelDir}"

    ${voicePromptRuntime}

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

  home = lib.mkIf pkgs.stdenv.isLinux {
    pointerCursor = {
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
    file.".local/bin/voxinput" = {
      source = "${voxinputClipboard}/bin/voxinput";
      executable = true;
    };
    file.".config/voxinput/terms.txt".text = voiceTermsText;
    packages =
      (with pkgs; [
        # Linux-only GUI/desktop
        aspell
        antigravity
        kiro
        aspellDicts.en
        supabase-cli
        albert
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
        feishu
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
        dotool
        openai-whisper
        pavucontrol
        sox
        voxinputClipboard
        whisperCppPackage
        wl-clipboard
        ydotool

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
      ++ lib.optionals pkgs.stdenv.isLinux (
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

  systemd.user.services.whisper-cpp-server = lib.mkIf pkgs.stdenv.isLinux {
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

  sops.secrets."voxinput/terms" = lib.mkIf (pkgs.stdenv.isLinux && !isRoot) { };

  systemd.user.services.voxinput-listener = lib.mkIf pkgs.stdenv.isLinux {
    Unit = {
      Description = "VoxInput push-to-talk listener";
      After = [
        "pipewire.service"
        "whisper-cpp-server.service"
      ];
      Wants = [ "whisper-cpp-server.service" ];
    };

    Service = {
      ExecStart = "${voxinputListener}";
      Restart = "on-failure";
      RestartSec = 2;
    };

    Install = {
      WantedBy = [ "default.target" ];
    };
  };

  gtk = lib.mkIf pkgs.stdenv.isLinux {
    enable = true;
    gtk4 = {
      extraConfig = {
        gtk-cursor-blink = false;
        gtk-recent-files-limit = 20;
      };
    };
  };

  i18n = lib.mkIf pkgs.stdenv.isLinux {
    inputMethod = {
      type = "fcitx5";
      enable = true;
      fcitx5 = {
        addons = with pkgs; [
          #fcitx5-mozc
          fcitx5-gtk
          #fcitx5-chinese-addons
          fcitx5-rime
        ];
      };
    };
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

    obs-studio = lib.mkIf pkgs.stdenv.isLinux {
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
