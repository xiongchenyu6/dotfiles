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
      defaultEditor = false;  # Set to false to avoid conflict with emacs
      viAlias = true;
      vimAlias = true;
      vimdiffAlias = true;

      # Use the neovim from nixpkgs
      package = pkgs.neovim-unwrapped;

      # Enable useful options through home-manager
      withNodeJs = true;
      withPython3 = true;
      withRuby = true;

      # COC (Completion) support - disabled as we're using nvim-cmp
      # coc.enable = false;

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

        -- Load configuration files from stow-managed directory
        local config_path = vim.fn.expand("~/dotfiles/stow-managed/config/.config/nvim/lua")
        if vim.fn.isdirectory(config_path) == 1 then
          vim.opt.rtp:prepend(vim.fn.expand("~/dotfiles/stow-managed/config/.config/nvim"))
          
          -- Try to load configuration files if they exist
          local ok_options = pcall(require, "config.options")
          local ok_keymaps = pcall(require, "config.keymaps")
          local ok_autocmds = pcall(require, "config.autocmds")
        end
        
        -- Basic plugin configurations since we're not using lazy.nvim
        -- Load colorscheme
        pcall(function()
          require("tokyonight").setup({
            style = "moon",
            transparent = true,
            terminal_colors = true,
            styles = {
              comments = { italic = true },
              keywords = { italic = true },
            },
          })
          vim.cmd.colorscheme("tokyonight")
        end)
        
        -- Configure essential plugins with their basic setup
        pcall(function() require("nvim-treesitter.configs").setup({
          highlight = { enable = true },
          indent = { enable = true },
          incremental_selection = { enable = true },
        }) end)
        
        pcall(function() require("nvim-tree").setup({
          view = { width = 30 },
          renderer = { group_empty = true },
        }) end)
        
        pcall(function() require("lualine").setup({
          options = { theme = "tokyonight", globalstatus = true },
        }) end)
        
        pcall(function() require("gitsigns").setup() end)
        pcall(function() require("which-key").setup() end)
        pcall(function() require("telescope").setup() end)
      '';

      # Plugins managed through home-manager instead of lazy.nvim
      plugins = with pkgs.vimPlugins; [
        # Colorschemes
        tokyonight-nvim
        catppuccin-nvim

        # LSP and completion
        nvim-lspconfig
        nvim-cmp
        cmp-nvim-lsp
        cmp-buffer
        cmp-path
        cmp-cmdline
        cmp_luasnip
        cmp-nvim-lua
        luasnip
        friendly-snippets
        mason-nvim
        mason-lspconfig-nvim
        mason-tool-installer-nvim
        fidget-nvim
        neodev-nvim

        # Formatting and linting
        conform-nvim
        nvim-lint

        # Treesitter
        nvim-treesitter.withAllGrammars
        nvim-treesitter-textobjects
        nvim-ts-context-commentstring
        nvim-treesitter-context
        nvim-ts-autotag

        # Editor enhancements
        mini-pairs
        mini-surround
        mini-comment
        mini-ai
        mini-indentscope

        # UI improvements
        dressing-nvim
        lualine-nvim
        nvim-web-devicons
        indent-blankline-nvim
        alpha-nvim
        nvim-notify
        noice-nvim
        nui-nvim

        # File management
        nvim-tree-lua
        telescope-nvim
        telescope-fzf-native-nvim
        plenary-nvim

        # Git integration
        gitsigns-nvim
        vim-fugitive
        vim-rhubarb
        diffview-nvim
        git-blame-nvim

        # Utilities
        which-key-nvim
        persistence-nvim
        trouble-nvim
        todo-comments-nvim
        nvim-colorizer-lua
        vim-illuminate
        flash-nvim
        nvim-spectre
        undotree
        vim-repeat
        nvim-surround
        comment-nvim
        nvim-bqf
        copilot-vim

        # Terminal integration
        toggleterm-nvim

        # Development tools
        vim-wakatime
        vim-startuptime
        treesj
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
