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
      defaultEditor = true;
      viAlias = true;
      vimAlias = true;
      vimdiffAlias = true;

      # Use the neovim from nixpkgs
      package = pkgs.neovim-unwrapped;

      # Enable useful options through home-manager
      withNodeJs = true;
      withPython3 = true;
      withRuby = true;

      # COC (Completion) support - if you want to use coc.nvim
      # coc.enable = false;  # We're using lazy.nvim instead

      # Configure through extraLuaConfig
      extraLuaConfig = ''
        -- Basic settings (that aren't covered by home-manager options)
        vim.opt.termguicolors = true
        vim.opt.mouse = 'a'
        vim.opt.clipboard = 'unnamedplus'
        vim.opt.number = true
        vim.opt.relativenumber = true
        vim.opt.signcolumn = 'yes'
        vim.opt.expandtab = true
        vim.opt.shiftwidth = 2
        vim.opt.tabstop = 2
        vim.opt.smartindent = true
        vim.opt.wrap = false
        vim.opt.swapfile = false
        vim.opt.backup = false
        vim.opt.undofile = true
        vim.opt.undodir = vim.fn.expand("~/.vim/undodir")
        vim.opt.hlsearch = false
        vim.opt.incsearch = true
        vim.opt.scrolloff = 8
        vim.opt.updatetime = 50
        vim.opt.colorcolumn = "80"

        -- Set leader key
        vim.g.mapleader = " "
        vim.g.maplocalleader = " "

        -- Bootstrap lazy.nvim
        local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
        if not vim.loop.fs_stat(lazypath) then
          vim.fn.system({
            "git",
            "clone",
            "--filter=blob:none",
            "https://github.com/folke/lazy.nvim.git",
            "--branch=stable",
            lazypath,
          })
        end
        vim.opt.rtp:prepend(lazypath)

        -- Load configuration files from stow-managed directory
        local config_path = vim.fn.expand("~/dotfiles/stow-managed/config/.config/nvim/lua")
        if vim.fn.isdirectory(config_path) == 1 then
          vim.opt.rtp:prepend(vim.fn.expand("~/dotfiles/stow-managed/config/.config/nvim"))
          
          -- Try to load configuration files if they exist
          local ok_options = pcall(require, "config.options")
          local ok_keymaps = pcall(require, "config.keymaps")
          local ok_autocmds = pcall(require, "config.autocmds")
          
          -- Setup lazy.nvim and load plugins
          require("lazy").setup("plugins", {
            defaults = {
              lazy = false,
              version = false,
            },
            checker = { enabled = true },
            performance = {
              rtp = {
                disabled_plugins = {
                  "gzip",
                  "tarPlugin",
                  "tohtml",
                  "tutor",
                  "zipPlugin",
                },
              },
            },
          })
        else
          -- Minimal setup if stow-managed config doesn't exist
          require("lazy").setup({}, {
            defaults = {
              lazy = false,
              version = false,
            },
          })
        end
      '';

      # Plugins can be managed through home-manager or lazy.nvim
      # We're using lazy.nvim for now, but you can add plugins here too
      plugins = with pkgs.vimPlugins; [
        # Add any plugins you want to manage through Nix here
        # Example: nvim-treesitter
      ];

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
