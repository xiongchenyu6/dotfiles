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
        inputs.llm-agents.packages.${pkgs.system}.mcporter
        # inputs.llm-agents.packages.${pkgs.system}.cc-switch-cli # temporarily disabled: hash mismatch upstream
        inputs.llm-agents.packages.${pkgs.system}.auto-claude
        discord
        telegram-desktop
        cloc
        claude-monitor
        minicom
        doctl
        gdrive
        gtrash
        #freerdp
        #dmidecode
        # jetbrains.idea-ultimate
        # jetbrains.rider
        xournalpp
        slack
        zoom-us
        ueberzugpp
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
        #tpm2-tools

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
      ++ lib.optionals pkgs.stdenv.isLinux [
        google-chrome # Keep Chrome in Nix for Linux
        ledger-live-desktop # x86_64-linux only
        weave-gitops # Linux only
        calicoctl # Linux only
        jp2a # Marked broken on Darwin
        lm_sensors # Linux-only hardware monitoring
        fwupd # Firmware update daemon (Linux-only)
        gparted # Disk partitioning GUI (Linux-only)
        pciutils # PCI utilities (mostly Linux-specific)
      ];
  };

  programs = {
    ghostty = {
      enable = true;
      enableZshIntegration = true;
      systemd.enable = true;
      settings = {
        font-family = "Hack Nerd Font";
        font-size = 10;
        cursor-style = "bar";
        cursor-style-blink = true;
        background-opacity = 0.7;
        working-directory = "inherit";
        window-inherit-working-directory = true;
        shell-integration = "zsh";
      };
    };

    noti = {
      enable = true;
    };

  };
}
