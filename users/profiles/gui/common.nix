{ pkgs, ... }: {
  # Home Manager needs a bit of information about you and the
  # paths it should manage.

  home = {
    packages = with pkgs; [
      appimage-run
      bootstrap-studio
      discord
      dropbox
      freerdp
      jetbrains.idea-ultimate
      jdt-language-server
      opera
      mattermost-desktop
      microsoft-edge-dev
      xournal # note taking for pdf
      polypane
      slack
      tectonic
      wpsoffice
      zoom-us
    ];
  };

  programs = {
    alacritty = {
      enable = true;
      settings = {
        # opacity = 0.9;
        font = {
          size = 10;
          normal = { family = "Hack Nerd Font"; };
          bold = { family = "Hack Nerd Font"; };
          italic = { family = "Hack Nerd Font"; };
          bold_italic = { family = "Hack Nerd Font"; };
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

    noti = { enable = true; };

    emacs = {
      enable = true;
      extraPackages = epkgs:
        with pkgs;
        with epkgs; [
          ace-link
          all-the-icons
          blamer
          clojure-mode
          cider
          citre
          cmake-mode
          cape
          corfu
          copilot-el
          combobulate
          citeproc
          dockerfile-mode
          direnv
          dap-mode
          doom-modeline
          emacsql-sqlite-builtin # for org-roam
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
          # nov
          ox-hugo
          ob-mermaid
          ob-restclient
          ox-pandoc
          # org-ai
          org-contrib
          org-roam
          org-re-reveal
          org-download
          # org-cv
          orderless
          plantuml-mode
          pdf-tools
          #          posframe
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
          #          tide
          tempel
          terraform-mode
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
