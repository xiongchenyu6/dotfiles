return {
  -- Tokyo Night theme
  {
    "folke/tokyonight.nvim",
    priority = 1000,
    config = function()
      local transparent = true
      local tokyonight = require("tokyonight")
      tokyonight.setup({
        style = "moon",
        transparent = transparent,
        terminal_colors = true,
        styles = {
          comments = { italic = true },
          keywords = { italic = true },
          functions = {},
          variables = {},
          sidebars = transparent and "transparent" or "dark",
          floats = transparent and "transparent" or "dark",
        },
        on_colors = function(colors)
          colors.border = colors.blue0
        end,
      })
      tokyonight.load()
    end,
  },

  -- Catppuccin theme (alternative)
  {
    "catppuccin/nvim",
    name = "catppuccin",
    enabled = false,
    priority = 1000,
    config = function()
      require("catppuccin").setup({
        flavour = "mocha",
        transparent_background = true,
        integrations = {
          cmp = true,
          gitsigns = true,
          nvimtree = true,
          telescope = true,
          notify = true,
          mini = true,
        },
      })
    end,
  },
}