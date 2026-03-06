# Standalone emacs configuration - extracted from various host modules.
# Not imported by any host. Import this module to re-enable emacs.
#
# Contains:
#   - programs.emacs (packages + config) from gui/packages.nix
#   - services.emacs (daemon) from gui/linux.nix & nixos-integration.nix
#   - Platform-specific package overrides from hyprland/ and xmonad/
{ pkgs, lib, ... }:
{
  programs.emacs = {
    enable = true;

    # Override package per desktop environment:
    #   Hyprland (Wayland) -> emacs30-pgtk
    #   XMonad   (X11)     -> emacs29
    # Uncomment the one matching your setup, or leave default.
    # package = pkgs.emacs30-pgtk;
    # package = pkgs.emacs29;

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

  services.emacs = {
    enable = true;
    defaultEditor = false; # Use neovim as default editor
    client = {
      enable = true;
    };
    socketActivation = {
      enable = false;
    };
  };
}
