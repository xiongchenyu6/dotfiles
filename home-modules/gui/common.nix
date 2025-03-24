{ pkgs, ... }:
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.

  home = {
    packages = with pkgs; [
      #appimage-run
      #discord
      doctl
      gdrive
      gtrash
      google-chrome
      #freerdp
      #dmidecode
      # jetbrains.idea-ultimate
      # jetbrains.rider
      xournalpp
      slack
      #onlyoffice-bin
      zoom-us
      ueberzugpp
      ytfzf
      usbutils
      #stlink-gui
      #stm32cubemx
      #gcc-arm-embedded
      microsoft-edge
      #vault
      solana-cli
      # expect mkpasswd conflict
      gpg-tui
      sysz
      ncdu
      lazygit
      lazydocker
      (warp-terminal.override { waylandSupport = true; })
      #kmon
      termshark
      glow
      lm_sensors
      fwupd
      lnav
      lego
      #gitbutler
      #unityhub
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
      gdb
      openfortivpn
      gnumake
      geoip
      manix
      grafana-loki
      gparted
      imagemagick
      inetutils
      ifuse
      jp2a
      lsof
      #my_cookies
      glib
      pass
      pciutils
      patchelf
      procs
      #qemu_kvm
      tpm2-tools
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
          ace-link
          all-the-icons
          blamer
          clojure-mode
          cider
          citre
          cmake-mode
          cape
          corfu
          emacs-copilot-el
          emacs-copilot-chat
          emacs-combobulate
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
          lsp-java
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
          rime
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
          treesit-grammars.with-all-grammars
          vterm
          wakatime-mode
          which-key
          yaml-mode
        ];
      extraConfig = ''
        (add-to-list 'default-frame-alist
        '(font . "JetBrains Mono-14"))
        (setq custom-file "~/.config/emacs/custom.el")
      '';
    };
  };
}
