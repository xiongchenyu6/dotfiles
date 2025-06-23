{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.modules.neovim;
in
{
  options.modules.neovim = {
    enable = lib.mkEnableOption "neovim";
  };

  config = lib.mkIf cfg.enable {
    programs.neovim = {
      enable = true;
      defaultEditor = false;
      viAlias = true;
      vimAlias = true;

      # Use the neovim from nixpkgs
      package = pkgs.neovim-unwrapped;

      # Since we're using stow-managed config, we don't need to configure plugins here
      # The actual configuration is in stow-managed/config/.config/nvim/

      # Set up some basic options that work well with the stow-managed config
      extraConfig = ''
        " Basic settings to ensure compatibility
        set termguicolors
        set mouse=a
        set clipboard+=unnamedplus

        " The main configuration is loaded from ~/.config/nvim/init.lua
        " which is managed by stow
      '';

      # Ensure we have the necessary runtime dependencies
      extraPackages = with pkgs; [
        # Language servers and tools
        lua-language-server
        nodePackages.typescript-language-server
        nodePackages.vscode-langservers-extracted
        pyright
        rust-analyzer
        gopls
        nil # Nix language server

        # Formatters and linters
        stylua
        nodePackages.prettier
        black
        rustfmt
        nixfmt-rfc-style

        # Tools used by plugins
        ripgrep
        fd
        fzf
        tree-sitter

        # Clipboard support
        wl-clipboard # for wayland
        xclip # for x11
      ];
    };
  };
}
