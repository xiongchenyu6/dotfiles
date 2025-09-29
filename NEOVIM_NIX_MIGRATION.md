# Neovim Nix Migration

This document outlines the migration from lazy.nvim to Nix-managed plugins for Neovim.

## Changes Made

### 1. Removed lazy.nvim Bootstrap
- Removed the lazy.nvim bootstrap code from `extraLuaConfig`
- Removed lazy.nvim setup calls

### 2. Added Nix-Managed Plugins
The following plugins are now managed through `home-modules/neovim.nix`:

#### Colorschemes
- `tokyonight-nvim`
- `catppuccin-nvim`

#### LSP and Completion
- `nvim-lspconfig`
- `nvim-cmp` and completion sources
- `luasnip` and `friendly-snippets`
- `mason-nvim`, `mason-lspconfig-nvim`, `mason-tool-installer-nvim`
- `fidget-nvim`, `neodev-nvim`

#### Formatting and Linting
- `conform-nvim`
- `nvim-lint`

#### Treesitter
- `nvim-treesitter.withAllGrammars`
- `nvim-treesitter-textobjects`
- `nvim-ts-context-commentstring`
- `nvim-treesitter-context`
- `nvim-ts-autotag`

#### Editor Enhancements
- Mini plugins: `mini-pairs`, `mini-surround`, `mini-comment`, `mini-ai`, `mini-indentscope`

#### UI Improvements
- `dressing-nvim`, `lualine-nvim`, `nvim-web-devicons`
- `indent-blankline-nvim`, `alpha-nvim`
- `nvim-notify`, `noice-nvim`, `nui-nvim`

#### File Management
- `nvim-tree-lua`
- `telescope-nvim`, `telescope-fzf-native-nvim`
- `plenary-nvim`

#### Git Integration
- `gitsigns-nvim`, `vim-fugitive`, `vim-rhubarb`
- `diffview-nvim`, `git-blame-nvim`

#### Utilities
- `which-key-nvim`, `persistence-nvim`, `trouble-nvim`
- `todo-comments-nvim`, `nvim-colorizer-lua`
- `vim-illuminate`, `flash-nvim`, `nvim-spectre`
- `undotree`, `vim-repeat`, `nvim-surround`
- `comment-nvim`, `nvim-bqf`, `copilot-vim`

#### Terminal and Development Tools
- `toggleterm-nvim`
- `vim-wakatime`, `vim-startuptime`
- `treesj`

### 3. Added Basic Plugin Configuration
Added minimal setup calls for essential plugins to ensure they work immediately:
- Tokyonight colorscheme setup and activation
- Treesitter basic configuration
- Essential plugin setups (nvim-tree, lualine, gitsigns, which-key, telescope)

## Configuration Files
The existing Lua configuration files in `stow-managed/config/.config/nvim/lua/plugins/` can still be used, but they need to be adapted from lazy.nvim format to direct plugin setup calls.

## Benefits of This Migration

1. **Reproducibility**: All plugins are managed through Nix, ensuring reproducible environments
2. **Version Control**: Plugin versions are pinned and managed through the Nix flake
3. **System Integration**: Better integration with the NixOS system
4. **Offline Usage**: Plugins are available even when offline
5. **Consistency**: Aligns with the overall Nix-based system configuration

## Migration Notes

- The lazy.nvim configuration files in `stow-managed/config/.config/nvim/lua/plugins/` contain plugin-specific configurations that may need to be adapted
- Some lazy.nvim specific features (like lazy loading) are handled differently in Nix
- Plugin keymaps and options from the original configurations should still work as they're loaded via the runtime path

## Usage

After rebuilding the home-manager configuration, Neovim will use the Nix-managed plugins instead of lazy.nvim. The existing configuration files will still be loaded, but plugin management is now handled by Nix.