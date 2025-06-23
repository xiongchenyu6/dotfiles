# Modern Neovim Configuration

This is a modern Neovim configuration using Lua and the latest best practices.

## Features

- **Plugin Manager**: [lazy.nvim](https://github.com/folke/lazy.nvim) - Modern, fast plugin manager
- **LSP**: Full Language Server Protocol support with auto-completion
- **Treesitter**: Advanced syntax highlighting and code understanding
- **Telescope**: Fuzzy finder for files, text, and more
- **Git Integration**: Fugitive, Gitsigns, and more
- **Modern UI**: Tokyo Night theme, statusline, dashboard

## Key Mappings

Leader key: `Space`

### Common Operations
- `<leader><space>` - Find files
- `<leader>fg` - Live grep
- `<leader>e` - File explorer
- `<leader>l` - Lazy plugin manager

### LSP
- `gd` - Go to definition
- `gr` - Find references
- `K` - Hover documentation
- `<leader>ca` - Code actions
- `<leader>cr` - Rename

### Git
- `<leader>gs` - Git status
- `<leader>gb` - Git blame
- `]h` / `[h` - Next/prev git hunk

### Window Management
- `<leader>w-` - Split horizontally
- `<leader>w|` - Split vertically
- `<C-h/j/k/l>` - Navigate windows

## Installation

1. Start Neovim
2. Lazy.nvim will auto-install on first run
3. Plugins will be automatically downloaded
4. Run `:Mason` to install LSP servers

## Customization

- Edit `lua/config/options.lua` for general settings
- Edit `lua/config/keymaps.lua` for key mappings
- Add new plugins in `lua/plugins/`
- Each plugin file should return a table of plugin specs

## Migrating from your old config

Your old configuration used:
- vim-plug → now using lazy.nvim
- Denite → now using Telescope
- ncm2 → now using nvim-cmp
- ale → now using built-in LSP diagnostics
- lightline → now using lualine
- vim syntax → now using Treesitter

Most of your key mappings have been preserved or improved.