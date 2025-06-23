return {
  -- LSP Configuration
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      "mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      {
        "j-hui/fidget.nvim",
        tag = "legacy",
        config = true,
      },
      "folke/neodev.nvim",
    },
    opts = {
      diagnostics = {
        underline = true,
        update_in_insert = false,
        virtual_text = {
          spacing = 4,
          source = "if_many",
          prefix = "●",
        },
        severity_sort = true,
      },
      inlay_hints = {
        enabled = true,
      },
      capabilities = {},
      format = {
        formatting_options = nil,
        timeout_ms = nil,
      },
      servers = {
        lua_ls = {
          settings = {
            Lua = {
              workspace = {
                checkThirdParty = false,
              },
              completion = {
                callSnippet = "Replace",
              },
              telemetry = { enable = false },
              diagnostics = {
                globals = { "vim" },
              },
            },
          },
        },
        rust_analyzer = {
          settings = {
            ["rust-analyzer"] = {
              cargo = {
                allFeatures = true,
              },
              checkOnSave = {
                command = "clippy",
              },
            },
          },
        },
        gopls = {
          settings = {
            gopls = {
              analyses = {
                unusedparams = true,
              },
              staticcheck = true,
              gofumpt = true,
            },
          },
        },
        pyright = {},
        tsserver = {},
        html = {},
        cssls = {},
        jsonls = {},
        yamlls = {},
        bashls = {},
        dockerls = {},
        marksman = {},
        clangd = {},
        cmake = {},
      },
      setup = {},
    },
    config = function(_, opts)
      -- Setup neodev for Neovim Lua API
      require("neodev").setup()

      -- Diagnostic config with signs
      vim.diagnostic.config(vim.tbl_deep_extend("force", opts.diagnostics, {
        signs = {
          text = {
            [vim.diagnostic.severity.ERROR] = " ",
            [vim.diagnostic.severity.WARN] = " ",
            [vim.diagnostic.severity.HINT] = " ",
            [vim.diagnostic.severity.INFO] = " ",
          },
        },
      }))

      -- LSP handlers
      vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
        border = "rounded",
      })

      vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
        border = "rounded",
      })

      -- Setup servers
      local servers = opts.servers
      local capabilities = vim.tbl_deep_extend(
        "force",
        {},
        vim.lsp.protocol.make_client_capabilities(),
        require("cmp_nvim_lsp").default_capabilities(),
        opts.capabilities or {}
      )

      local function setup(server)
        local server_opts = vim.tbl_deep_extend("force", {
          capabilities = vim.deepcopy(capabilities),
        }, servers[server] or {})

        if opts.setup[server] then
          if opts.setup[server](server, server_opts) then
            return
          end
        elseif opts.setup["*"] then
          if opts.setup["*"](server, server_opts) then
            return
          end
        end
        require("lspconfig")[server].setup(server_opts)
      end

      -- Setup mason-lspconfig
      local mlsp = require("mason-lspconfig")
      local all_servers = vim.tbl_keys(servers)

      mlsp.setup({
        ensure_installed = all_servers,
        handlers = { setup },
      })
    end,
    keys = {
      { "<leader>cd", vim.diagnostic.open_float, desc = "Line Diagnostics" },
      { "<leader>cl", "<cmd>LspInfo<cr>", desc = "Lsp Info" },
      { "gd", "<cmd>Telescope lsp_definitions<cr>", desc = "Goto Definition" },
      { "gr", "<cmd>Telescope lsp_references<cr>", desc = "References" },
      { "gD", vim.lsp.buf.declaration, desc = "Goto Declaration" },
      { "gI", "<cmd>Telescope lsp_implementations<cr>", desc = "Goto Implementation" },
      { "gy", "<cmd>Telescope lsp_type_definitions<cr>", desc = "Goto Type Definition" },
      { "K", vim.lsp.buf.hover, desc = "Hover" },
      { "gK", vim.lsp.buf.signature_help, desc = "Signature Help" },
      { "<c-k>", vim.lsp.buf.signature_help, mode = "i", desc = "Signature Help" },
      { "[d", vim.diagnostic.goto_prev, desc = "Prev Diagnostic" },
      { "]d", vim.diagnostic.goto_next, desc = "Next Diagnostic" },
      { "[e", function() vim.diagnostic.goto_prev({ severity = vim.diagnostic.severity.ERROR }) end, desc = "Prev Error" },
      { "]e", function() vim.diagnostic.goto_next({ severity = vim.diagnostic.severity.ERROR }) end, desc = "Next Error" },
      { "[w", function() vim.diagnostic.goto_prev({ severity = vim.diagnostic.severity.WARN }) end, desc = "Prev Warning" },
      { "]w", function() vim.diagnostic.goto_next({ severity = vim.diagnostic.severity.WARN }) end, desc = "Next Warning" },
      { "<leader>ca", vim.lsp.buf.code_action, desc = "Code Action", mode = { "n", "v" } },
      { "<leader>cf", function() vim.lsp.buf.format({ async = true }) end, desc = "Format" },
      { "<leader>cr", vim.lsp.buf.rename, desc = "Rename" },
    },
  },

  -- Package manager for LSP servers
  {
    "williamboman/mason.nvim",
    cmd = "Mason",
    build = ":MasonUpdate",
    keys = {
      { "<leader>cm", "<cmd>Mason<cr>", desc = "Mason" },
    },
    opts = {
      ui = {
        icons = {
          package_installed = "✓",
          package_pending = "➜",
          package_uninstalled = "✗",
        },
      },
    },
  },

  -- Formatting
  {
    "stevearc/conform.nvim",
    event = { "BufWritePre" },
    cmd = { "ConformInfo" },
    keys = {
      {
        "<leader>cF",
        function()
          require("conform").format({ async = true, lsp_fallback = true })
        end,
        mode = "",
        desc = "Format buffer",
      },
    },
    opts = {
      formatters_by_ft = {
        lua = { "stylua" },
        python = { "isort", "black" },
        javascript = { { "prettierd", "prettier" } },
        typescript = { { "prettierd", "prettier" } },
        javascriptreact = { { "prettierd", "prettier" } },
        typescriptreact = { { "prettierd", "prettier" } },
        json = { { "prettierd", "prettier" } },
        html = { { "prettierd", "prettier" } },
        css = { { "prettierd", "prettier" } },
        scss = { { "prettierd", "prettier" } },
        markdown = { { "prettierd", "prettier" } },
        yaml = { { "prettierd", "prettier" } },
        rust = { "rustfmt" },
        go = { "goimports", "gofmt" },
        sh = { "shfmt" },
      },
      format_on_save = function(bufnr)
        if vim.g.disable_autoformat or vim.b[bufnr].disable_autoformat then
          return
        end
        return { timeout_ms = 500, lsp_fallback = true }
      end,
    },
    init = function()
      vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
    end,
  },

  -- Linting
  {
    "mfussenegger/nvim-lint",
    event = { "BufReadPre", "BufNewFile" },
    config = function()
      local lint = require("lint")
      lint.linters_by_ft = {
        javascript = { "eslint_d" },
        typescript = { "eslint_d" },
        javascriptreact = { "eslint_d" },
        typescriptreact = { "eslint_d" },
        python = { "pylint" },
        go = { "golangcilint" },
        lua = { "luacheck" },
      }

      vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost", "InsertLeave" }, {
        callback = function()
          lint.try_lint()
        end,
      })
    end,
  },

  -- Mason tool installer
  {
    "WhoIsSethDaniel/mason-tool-installer.nvim",
    dependencies = "mason.nvim",
    cmd = { "MasonToolsInstall", "MasonToolsUpdate" },
    event = { "BufReadPre", "BufNewFile" },
    opts = {
      ensure_installed = {
        -- Lua
        "lua-language-server",
        "stylua",
        -- Python
        "pyright",
        "black",
        "isort",
        "pylint",
        -- Go
        "gopls",
        "goimports",
        "golangci-lint",
        -- JavaScript/TypeScript
        "typescript-language-server",
        "prettier",
        "prettierd",
        "eslint_d",
        -- Web
        "html-lsp",
        "css-lsp",
        "json-lsp",
        -- Other
        "bash-language-server",
        "shfmt",
        "yaml-language-server",
        "marksman",
        "dockerfile-language-server",
      },
    },
  },
}