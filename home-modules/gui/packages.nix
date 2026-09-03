{
  inputs,
  pkgs,
  lib,
  ...
}:
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home = {
    packages =
      with pkgs;
      [
        #appimage-run
        inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.mcporter
        inputs.xiongchenyu6.packages.${pkgs.stdenv.hostPlatform.system}.cc-switch
        inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.aperant
        inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.agentsview
        kitty.kitten # 独立的 kitten 二进制（icat/ssh/clipboard 等小工具），不装 kitty 本体
        bitwarden-desktop
        bitwarden-cli
        discord
        telegram-desktop
        cloc
        claude-monitor
        minicom
        doctl
        wrangler
        gdrive
        gtrash
        #freerdp
        #dmidecode
        # jetbrains.idea-ultimate
        # jetbrains.rider
        xournalpp
        slack
        zoom-us
        jitsi-meet-electron
        # 终端内看图/视频/PDF 的一套：yazi 包装器自带一份，这里再放进
        # PATH 是给 pi 之类 CLI agent 直接调用（ffmpeg-full、imagemagick 在下面）
        ueberzugpp
        chafa
        resvg
        poppler-utils
        #ytfzf
        usbutils
        zip
        #vault
        #solana-cli
        # expect mkpasswd conflict
        gpg-tui
        sysz
        ncdu
        lazygit
        lazydocker
        #(warp-terminal.override { waylandSupport = true; })
        #kmon
        termshark
        glow # markdown viewer
        lnav
        lego
        #gitbutler
        # zed-editor
        nixd
        #v4l-utils
        dotnetCorePackages.sdk_8_0
        foundry
        surfpool
        #record_screen
        apg
        #cava # audio visualizer
        cmake
        gcc
        openfortivpn
        gnumake
        geoip
        github-copilot-cli
        manix
        grafana-loki
        imagemagick
        inetutils
        #ifuse
        lsof
        #my_cookies
        glib
        pass
        patchelf
        procs
        ansible.out
        #qemu_kvm

        # Cross-platform GUI apps (moved from Linux-only)
        keepassxc
        zotero
        yubikey-manager

        # Cross-platform CLI/DevOps tools (moved from Linux-only)
        cloudflared
        sops
        gpgme.dev
        cmctl
        websocat
        delve # go debugger
        dive # docker image analyzer
        dust # du alternative
        envsubst
        ffmpeg-full
        fluxcd
        glab
        gitleaks
        graphviz
        grpcurl
        (kubernetes-helm-wrapped.override { plugins = [ kubernetes-helmPlugins.helm-diff ]; })
        #helm
        hey # http load generator
        killall
        litecli
        mongosh
        #mycli
        nix-index-update
        nixpacks
        nix
        nvfetcher
        oath-toolkit
        openssl
        popeye
        plantuml
        #aider-chat
        (python3.withPackages (
          _: with python3.pkgs; [
            pip
          ]
        ))
        python312Packages.huggingface-hub.out
        uv
        github-mcp-server
        qrencode
        redis
        ruby
        shellcheck
        shfmt
        ldns
        nmap # A utility for network discovery and security auditing
        ipcalc
        nix-fast-build
        solc-select
        #solium
        socat
        sshpass
        tgpt
        tealdeer
        unzip
        wakatime-cli
        wget
        ripgrep
        p7zip
        file
        exiftool
        jq

        # Kubernetes tools
        kube-capacity
        kube-prompt
        kubectl
        kubectl-tree
        kubespy
        kubeshark
        kustomize
        krew
        kconf
        #orb
        kube-score
        kubelogin-oidc
      ]
      ++ lib.optionals pkgs.stdenv.hostPlatform.isLinux [
        ledger-live-desktop # x86_64-linux only
        weave-gitops # Linux only
        calicoctl # Linux only
        jp2a # Marked broken on Darwin
        lm_sensors # Linux-only hardware monitoring
        amdgpu_top # AMD GPU usage monitor
        nvtopPackages.full # Multi-vendor GPU process monitor (AMD/NVIDIA)
        fwupd # Firmware update daemon (Linux-only)
        gparted # Disk partitioning GUI (Linux-only)
        pciutils # PCI utilities (mostly Linux-specific)
        tpm2-tools # TPM 2.0 工具,meta.platforms 只有 Linux
      ];
  };

  programs = {
    noti = {
      enable = true;
    };
  }
  // lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
    rio = {
      enable = true;
      settings = {
        # rio 自带的 xterm-rio terminfo 声明只有 8 色，zsh 的 %F{8} 之类
        # 直接不输出，自动补全的灰色会变白；改回通用的 256 色 TERM。
        env-vars = [ "TERM=xterm-256color" ];
        fonts = {
          family = "Hack Nerd Font";
          size = 14;
          # rio 的 emoji 回退走 fontconfig 单字符查找，会把 ☁️ 交给 CJK
          # 字体、😀 交给 FontAwesome；把 emoji 区段显式钉到 Noto Color Emoji。
          # 中文回退默认落到 Noto Sans CJK，它的 hhea ascent 是 1.16em 而 Hack 是
          # 0.93em，rio 按各字体自己的 ascent 定基线，中文会比英文低几像素。
          # 更纱黑体（0.97em）是给终端配的，基线基本对齐，且系统里已经装了。
          symbol-map =
            map
              (r: {
                inherit (r) start end;
                font-family = "Sarasa Mono SC";
              })
              [
                {
                  start = "2E80";
                  end = "9FFF";
                }
                {
                  start = "F900";
                  end = "FAFF";
                }
                {
                  start = "FF00";
                  end = "FFEF";
                }
              ]
            ++
              map
                (r: {
                  inherit (r) start end;
                  font-family = "Noto Color Emoji";
                })
                [
                  {
                    start = "2600";
                    end = "2604";
                  }
                  {
                    start = "26A0";
                    end = "26A1";
                  }
                  {
                    start = "2705";
                    end = "2705";
                  }
                  {
                    start = "2728";
                    end = "2728";
                  }
                  {
                    start = "274C";
                    end = "274C";
                  }
                  {
                    start = "2764";
                    end = "2764";
                  }
                  {
                    start = "1F300";
                    end = "1F64F";
                  }
                  {
                    start = "1F680";
                    end = "1F6FF";
                  }
                  {
                    start = "1F900";
                    end = "1F9FF";
                  }
                  {
                    start = "1FA70";
                    end = "1FAFF";
                  }
                ];
        };
        cursor = {
          shape = "beam";
          blinking = true;
        };
        window = {
          opacity = 0.9;
          blur = false;
          # niri 没有服务端装饰，rio 会自己画一条带最小化/最大化/关闭的标题栏
          decorations = "disabled";
        };
        shell = {
          program = "zsh";
          args = [ ];
        };
        confirm-before-quit = false;
        # 标签栏标题只按首字符选字体，中文标题会变方块（上游 bug）；
        # 彻底不画标签栏，标签靠快捷键切换。
        navigation.mode = "plain";
      };
    };
  };
}
