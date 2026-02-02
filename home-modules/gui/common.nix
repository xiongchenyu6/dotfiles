{ pkgs, lib, ... }:
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.

  home = {
    packages =
      with pkgs;
      [
        #appimage-run
        #discord
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
        #sui
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
        jp2a
        lsof
        #my_cookies
        glib
        pass
        patchelf
        procs
        ansible.out
        #qemu_kvm
        #tpm2-tools
      ]
      ++ lib.optionals pkgs.stdenv.isLinux [
        google-chrome # Keep Chrome in Nix for Linux
        lm_sensors # Linux-only hardware monitoring
        fwupd # Firmware update daemon (Linux-only)
        gparted # Disk partitioning GUI (Linux-only)
        pciutils # PCI utilities (mostly Linux-specific)
      ];
  };

  programs = {
    alacritty = {
      enable = true;
      settings = {
        # opacity = 0.9;
        font = {
          size = 10;
          normal = {
            family = "Hack Nerd Font";
          };
          bold = {
            family = "Hack Nerd Font";
          };
          italic = {
            family = "Hack Nerd Font";
          };
          bold_italic = {
            family = "Hack Nerd Font";
          };
        };
        cursor = {
          style = {
            shape = "Beam";
            blinking = "Always";
          };
        };
        keyboard = {
          bindings = [
            {
              key = "Space";
              mods = "Control|Shift";
              mode = "~Search";
              action = "ToggleViMode";
            }
            {
              key = "Return";
              mods = "Command|Shift";
              action = "SpawnNewInstance";
            }
          ];
        };
      };
    };

    noti = {
      enable = true;
    };

    emacs = {
      enable = true;
      extraPackages =
        epkgs:
        with pkgs;
        with epkgs;
        [
          aidermacs
          ace-link
          all-the-icons
          blamer
          # clojure-mode
          # cider
          citre
          cmake-mode
          cape
          corfu
          emacs-copilot-el
          emacs-combobulate
          #emacs-gptel
          citeproc
          dockerfile-mode
          direnv
          dap-mode
          doom-modeline
          format-all
          gradle-mode
          graphviz-dot-mode
          gnu-apl-mode
          gcmh
          go-mode
          haskell-mode
          kind-icon
          leetcode
          lispy
          lsp-haskell
          nix-mode
          magit
          magit-todos
          marginalia
          magit-gitflow
          ox-hugo
          ob-mermaid
          ob-restclient
          ox-pandoc
          org-contrib
          org-roam
          org-re-reveal
          org-download
          orderless
          plantuml-mode
          pdf-tools
          protobuf-mode
          restclient
          rainbow-delimiters
          racket-mode
          reformatter
          rg
          rust-mode
          solidity-mode
          sly
          solidity-flycheck
          scala-mode
          sbt-mode
          tempel
          #terraform-mode
          (treesit-grammars.with-grammars (
            ps: with ps; [
              tree-sitter-c
              tree-sitter-cpp
              tree-sitter-rust
              tree-sitter-python
              tree-sitter-javascript
              tree-sitter-typescript
              tree-sitter-tsx
              tree-sitter-css
              tree-sitter-html
              tree-sitter-json
              tree-sitter-yaml
              tree-sitter-markdown
              tree-sitter-nix
              tree-sitter-bash
              tree-sitter-go
              tree-sitter-java
              tree-sitter-scala
              tree-sitter-haskell
              tree-sitter-dockerfile
              tree-sitter-sql
              tree-sitter-toml
              # Excluding tree-sitter-razor due to build issues
            ]
          ))
          vterm
          wakatime-mode
          which-key
          yaml-mode
        ]
        ++ lib.optionals pkgs.stdenv.isLinux [
          rime # Chinese input method - Linux only due to architecture issues on Darwin
        ];
      extraConfig = ''
        (add-to-list 'default-frame-alist
        '(font . "JetBrains Mono-14"))
        (setq custom-file "~/.config/emacs/custom.el")
        (setq plantuml-executable-path "${pkgs.plantuml}/bin/plantuml")
      '';
    };
  };
}
