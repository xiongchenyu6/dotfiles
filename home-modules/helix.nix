{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.helix;
in
{
  options.modules.helix = {
    enable = lib.mkEnableOption "helix editor";
  };

  config = lib.mkIf cfg.enable {
    programs.helix = {
      enable = true;

      # Use the helix from nixpkgs
      package = pkgs.helix;

      settings = {
        theme = "tokyonight";

        editor = {
          # Line numbers
          line-number = "relative";
          cursorline = true;

          # Indentation
          indent-guides = {
            render = true;
            character = "▏";
          };

          # UI
          scrolloff = 8;
          mouse = true;
          middle-click-paste = true;
          scroll-lines = 3;
          shell = [
            "bash"
            "-c"
          ];
          completion-trigger-len = 2;
          auto-completion = true;
          auto-format = true;
          auto-save = false;
          idle-timeout = 400;

          # Search
          search = {
            smart-case = true;
            wrap-around = true;
          };

          # Whitespace
          whitespace = {
            render = {
              tab = "all";
              space = "none";
              newline = "none";
            };
            characters = {
              tab = "→";
              tabpad = "·";
            };
          };

          # LSP
          lsp = {
            display-messages = true;
            display-inlay-hints = true;
          };

          # File picker
          file-picker = {
            hidden = false;
            git-ignore = true;
            git-global = true;
            git-exclude = true;
          };

          # Status line
          statusline = {
            left = [
              "mode"
              "spinner"
              "file-name"
              "file-modification-indicator"
            ];
            center = [ "position" ];
            right = [
              "diagnostics"
              "selections"
              "position-percentage"
              "file-encoding"
              "file-line-ending"
              "file-type"
            ];
            separator = "│";
          };

          # Cursor shape
          cursor-shape = {
            insert = "bar";
            normal = "block";
            select = "underline";
          };
        };

        # Key mappings - similar to vim/neovim
        keys.normal = {
          # Quick save and quit
          "C-s" = ":w";
          "C-q" = ":q";

          # Window navigation
          "C-h" = "jump_view_left";
          "C-j" = "jump_view_down";
          "C-k" = "jump_view_up";
          "C-l" = "jump_view_right";

          # Tab navigation
          "H" = "goto_previous_buffer";
          "L" = "goto_next_buffer";

          # Center cursor after movements
          "C-d" = [
            "half_page_down"
            "align_view_center"
          ];
          "C-u" = [
            "half_page_up"
            "align_view_center"
          ];
          "n" = [
            "search_next"
            "align_view_center"
          ];
          "N" = [
            "search_prev"
            "align_view_center"
          ];

          # File tree
          space.e = "file_picker";
          space.E = "file_picker_in_current_directory";

          # LSP
          space.l = {
            a = "code_action";
            r = "rename_symbol";
            f = "format_selections";
            h = "hover";
            s = "symbol_picker";
            S = "workspace_symbol_picker";
            d = "diagnostics_picker";
            D = "workspace_diagnostics_picker";
            g = "goto_definition";
            G = "goto_type_definition";
            i = "goto_implementation";
            R = "goto_reference";
          };

          # Git
          space.g = {
            s = ":sh git status";
            l = ":sh git log --oneline";
            d = ":sh git diff";
            b = ":sh git blame";
          };
        };

        keys.insert = {
          # Escape with jk
          "j" = {
            "k" = "normal_mode";
          };
        };
      };

      # Language configuration
      languages = {
        language-server = {
          # Nix
          nil = {
            command = "${pkgs.nil}/bin/nil";
          };

          # Rust
          rust-analyzer = {
            command = "${pkgs.rust-analyzer}/bin/rust-analyzer";
            config = {
              checkOnSave.command = "clippy";
            };
          };

          # TypeScript/JavaScript
          typescript-language-server = {
            command = "${pkgs.nodePackages.typescript-language-server}/bin/typescript-language-server";
            args = [ "--stdio" ];
          };

          # Python
          pyright = {
            command = "${pkgs.pyright}/bin/pyright-langserver";
            args = [ "--stdio" ];
          };

          # Go
          gopls = {
            command = "${pkgs.gopls}/bin/gopls";
          };

          # Lua
          lua-language-server = {
            command = "${pkgs.lua-language-server}/bin/lua-language-server";
          };

          # YAML
          yaml-language-server = {
            command = "${pkgs.nodePackages.yaml-language-server}/bin/yaml-language-server";
            args = [ "--stdio" ];
          };

          # JSON
          vscode-json-language-server = {
            command = "${pkgs.nodePackages.vscode-langservers-extracted}/bin/vscode-json-language-server";
            args = [ "--stdio" ];
          };

          # HTML/CSS
          vscode-html-language-server = {
            command = "${pkgs.nodePackages.vscode-langservers-extracted}/bin/vscode-html-language-server";
            args = [ "--stdio" ];
          };

          vscode-css-language-server = {
            command = "${pkgs.nodePackages.vscode-langservers-extracted}/bin/vscode-css-language-server";
            args = [ "--stdio" ];
          };
        };

        language = [
          {
            name = "nix";
            auto-format = true;
            formatter = {
              command = "${pkgs.nixfmt-rfc-style}/bin/nixfmt";
            };
          }
          {
            name = "rust";
            auto-format = true;
            formatter = {
              command = "${pkgs.rustfmt}/bin/rustfmt";
              args = [
                "--edition"
                "2021"
              ];
            };
          }
          {
            name = "python";
            auto-format = true;
            formatter = {
              command = "${pkgs.black}/bin/black";
              args = [
                "-"
                "--quiet"
              ];
            };
          }
          {
            name = "javascript";
            auto-format = true;
            formatter = {
              command = "${pkgs.nodePackages.prettier}/bin/prettier";
              args = [
                "--parser"
                "babel"
              ];
            };
          }
          {
            name = "typescript";
            auto-format = true;
            formatter = {
              command = "${pkgs.nodePackages.prettier}/bin/prettier";
              args = [
                "--parser"
                "typescript"
              ];
            };
          }
          {
            name = "json";
            auto-format = true;
            formatter = {
              command = "${pkgs.nodePackages.prettier}/bin/prettier";
              args = [
                "--parser"
                "json"
              ];
            };
          }
          {
            name = "yaml";
            auto-format = true;
            formatter = {
              command = "${pkgs.nodePackages.prettier}/bin/prettier";
              args = [
                "--parser"
                "yaml"
              ];
            };
          }
          {
            name = "markdown";
            auto-format = true;
            formatter = {
              command = "${pkgs.nodePackages.prettier}/bin/prettier";
              args = [
                "--parser"
                "markdown"
              ];
            };
          }
          {
            name = "lua";
            auto-format = true;
            formatter = {
              command = "${pkgs.stylua}/bin/stylua";
              args = [ "-" ];
            };
          }
        ];
      };

      # Themes are built-in to helix, no need to manually install
    };

    # Ensure we have the necessary runtime dependencies
    home.packages = with pkgs; [
      # Language servers (most are already in neovim module, but let's ensure they're available)
      lua-language-server
      nodePackages.typescript-language-server
      nodePackages.vscode-langservers-extracted
      nodePackages.yaml-language-server
      pyright
      rust-analyzer
      gopls
      nil

      # Formatters
      stylua
      nodePackages.prettier
      black
      rustfmt
      nixfmt-rfc-style

      # Tools
      ripgrep
      fd
      fzf
      tree-sitter

      # Clipboard support
      wl-clipboard
      xclip
    ];

    # Add hx alias
    programs.bash.shellAliases.hx = "helix";
    programs.zsh.shellAliases.hx = "helix";
  };
}
