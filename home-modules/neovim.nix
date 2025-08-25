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

      # Source the actual configuration from stow-managed directory
      extraLuaConfig = ''
        -- Load the stow-managed configuration if it exists
        local stow_config = vim.fn.expand("~/dotfiles/stow-managed/config/.config/nvim/init.lua")
        if vim.fn.filereadable(stow_config) == 1 then
          dofile(stow_config)
        end
      '';

      # Ensure we have the necessary runtime dependencies
      extraPackages = with pkgs; [
        # Language servers and tools
        lua-language-server
        nodePackages.typescript-language-server
        nodePackages.vscode-langservers-extracted
        pyright
        #rust-analyzer
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
        xclip # for x11
      ] ++ lib.optionals pkgs.stdenv.isLinux [
        wl-clipboard # for wayland (Linux only)
      ];
    };
  };
}
