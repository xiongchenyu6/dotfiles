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

      # Complete Neovim configuration migrated from ~/.config/nvim/lua
      extraLuaConfig = ''
        -- Set leader keys first
        vim.g.mapleader = " ";
        vim.g.maplocalleader = " ";

        -- ========== OPTIONS (from config/options.lua) ==========
        local opt = vim.opt

        -- UI
        opt.number = true
        opt.relativenumber = true
        opt.termguicolors = true
        opt.signcolumn = "yes";
        opt.showmode = false
        opt.showcmd = true
        opt.cmdheight = 1
        opt.laststatus = 3
        opt.scrolloff = 8
        opt.sidescrolloff = 8
        opt.pumheight = 10
        opt.pumblend = 10
        opt.winblend = 0
        opt.wrap = false
        opt.linebreak = true
        opt.list = true
        opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" };
        opt.fillchars = {
          vert = "│",
          fold = "⠀",
          eob = " ",
          msgsep = "‾",
          foldopen = "▾",
          foldsep = "│",
          foldclose = "▸",
        };

        -- Editing
        opt.expandtab = true
        opt.shiftwidth = 2
        opt.tabstop = 2
        opt.softtabstop = 2
        -- opt.smartindent = true
        opt.breakindent = true
        opt.undofile = true
        opt.undolevels = 10000
        opt.swapfile = false
        opt.backup = false
        opt.writebackup = false

        -- Search
        opt.ignorecase = true
        opt.smartcase = true
        opt.incsearch = true
        opt.hlsearch = true

        -- Performance
        opt.updatetime = 200
        opt.timeout = true
        opt.timeoutlen = 300
        opt.redrawtime = 1500
        opt.lazyredraw = true

        -- Split windows
        opt.splitright = true
        opt.splitbelow = true
        opt.splitkeep = "screen";

        -- Fold
        opt.foldlevel = 99
        opt.foldlevelstart = 99
        opt.foldenable = true
        opt.foldmethod = "expr"
        opt.foldexpr = "nvim_treesitter#foldexpr()"

        -- Other
        opt.mouse = "a"
        opt.clipboard = "unnamedplus"
        opt.fileencoding = "utf-8"
        opt.conceallevel = 0
        opt.hidden = true
        opt.autowrite = true
        opt.autoread = true
        opt.spell = true
        opt.spelllang = { "en" };
        opt.completeopt = { "menuone", "noselect", "noinsert" };
        opt.wildmode = "longest:full,full"
        opt.shortmess:append({ W = true, I = true, c = true, sI = true })
        opt.iskeyword:append("-")
        opt.formatoptions:remove({ "c", "r" })
        opt.runtimepath:remove("/usr/share/vim/vimfiles")
        opt.whichwrap:append("<>[]hl")

        -- Disable some builtin providers
        vim.g.loaded_node_provider = 0
        vim.g.loaded_python3_provider = 0
        vim.g.loaded_perl_provider = 0
        vim.g.loaded_ruby_provider = 0
        vim.g.markdown_recommended_style = 0

        -- ========== KEYMAPS (from config/keymaps.lua) ==========
        local map = vim.keymap.set

        -- Better up/down
        map({ "n", "x" }, "j", "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })
        map({ "n", "x" }, "k", "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })

        -- Move to window using the <ctrl> hjkl keys
        map("n", "<C-h>", "<C-w>h", { desc = "Go to left window", remap = true })
        map("n", "<C-j>", "<C-w>j", { desc = "Go to lower window", remap = true })
        map("n", "<C-k>", "<C-w>k", { desc = "Go to upper window", remap = true })
        map("n", "<C-l>", "<C-w>l", { desc = "Go to right window", remap = true })

        -- Resize window using <ctrl> arrow keys
        map("n", "<C-Up>", "<cmd>resize +2<cr>", { desc = "Increase window height" })
        map("n", "<C-Down>", "<cmd>resize -2<cr>", { desc = "Decrease window height" })
        map("n", "<C-Left>", "<cmd>vertical resize -2<cr>", { desc = "Decrease window width" })
        map("n", "<C-Right>", "<cmd>vertical resize +2<cr>", { desc = "Increase window width" })

        -- Move Lines
        map("n", "<A-j>", "<cmd>m .+1<cr>==", { desc = "Move down" })
        map("n", "<A-k>", "<cmd>m .-2<cr>==", { desc = "Move up" })
        map("i", "<A-j>", "<esc><cmd>m .+1<cr>==gi", { desc = "Move down" })
        map("i", "<A-k>", "<esc><cmd>m .-2<cr>==gi", { desc = "Move up" })
        map("v", "<A-j>", ":m '>+1<cr>gv=gv", { desc = "Move down" })
        map("v", "<A-k>", ":m '<-2<cr>gv=gv", { desc = "Move up" })

        -- Buffers
        map("n", "<S-h>", "<cmd>bprevious<cr>", { desc = "Prev buffer" })
        map("n", "<S-l>", "<cmd>bnext<cr>", { desc = "Next buffer" })
        map("n", "[b", "<cmd>bprevious<cr>", { desc = "Prev buffer" })
        map("n", "]b", "<cmd>bnext<cr>", { desc = "Next buffer" })
        map("n", "<leader>bb", "<cmd>e #<cr>", { desc = "Switch to Other Buffer" })
        map("n", "<leader>bd", "<cmd>bdelete<cr>", { desc = "Delete Buffer" })
        map("n", "<leader>bD", "<cmd>:bd<cr>", { desc = "Delete Buffer and Window" })

        -- Clear search with <esc>
        map({ "i", "n" }, "<esc>", "<cmd>noh<cr><esc>", { desc = "Escape and clear hlsearch" })

        -- Clear search, diff update and redraw
        map("n", "<leader>ur", "<Cmd>nohlsearch<Bar>diffupdate<Bar>normal! <C-L><CR>", { desc = "Redraw / clear hlsearch / diff update" })

        -- Save file
        map({ "i", "v", "n", "s" }, "<C-s>", "<cmd>w<cr><esc>", { desc = "Save file" })

        -- Better indenting
        map("v", "<", "<gv")
        map("v", ">", ">gv")

        -- New file
        map("n", "<leader>fn", "<cmd>enew<cr>", { desc = "New File" })

        -- Quit
        map("n", "<leader>qq", "<cmd>qa<cr>", { desc = "Quit all" })

        -- Highlights under cursor
        map("n", "<leader>ui", vim.show_pos, { desc = "Inspect Pos" })

        -- Windows
        map("n", "<leader>ww", "<C-W>p", { desc = "Other window", remap = true })
        map("n", "<leader>wd", "<C-W>c", { desc = "Delete window", remap = true })
        map("n", "<leader>w-", "<C-W>s", { desc = "Split window below", remap = true })
        map("n", "<leader>w|", "<C-W>v", { desc = "Split window right", remap = true })
        map("n", "<leader>-", "<C-W>s", { desc = "Split window below", remap = true })
        map("n", "<leader>|", "<C-W>v", { desc = "Split window right", remap = true })

        -- Tabs
        map("n", "<leader><tab>l", "<cmd>tablast<cr>", { desc = "Last Tab" })
        map("n", "<leader><tab>f", "<cmd>tabfirst<cr>", { desc = "First Tab" })
        map("n", "<leader><tab><tab>", "<cmd>tabnew<cr>", { desc = "New Tab" })
        map("n", "<leader><tab>]", "<cmd>tabnext<cr>", { desc = "Next Tab" })
        map("n", "<leader><tab>d", "<cmd>tabclose<cr>", { desc = "Close Tab" })
        map("n", "<leader><tab>[", "<cmd>tabprevious<cr>", { desc = "Previous Tab" })

        -- Terminal Mappings
        map("t", "<esc><esc>", "<c-\"><c-n>", { desc = "Enter Normal Mode" })
        map("t", "<C-h>", "<cmd>wincmd h<cr>", { desc = "Go to left window" })
        map("t", "<C-j>", "<cmd>wincmd j<cr>", { desc = "Go to lower window" })
        map("t", "<C-k>", "<cmd>wincmd k<cr>", { desc = "Go to upper window" })
        map("t", "<C-l>", "<cmd>wincmd l<cr>", { desc = "Go to right window" })
        map("t", "<C-/>", "<cmd>close<cr>", { desc = "Hide Terminal" })
        map("t", "<c-_>", "<cmd>close<cr>", { desc = "which_key_ignore" })

        -- Plugin-specific keymaps
        -- File explorer
        map("n", "<leader>e", "<cmd>NvimTreeToggle<cr>", { desc = "Explorer" })
        map("n", "<leader>fe", "<cmd>NvimTreeFindFile<cr>", { desc = "Find current file" })

        -- Telescope
        map("n", "<leader><space>", "<cmd>Telescope find_files<cr>", { desc = "Find files" })
        map("n", "<leader>ff", "<cmd>Telescope find_files<cr>", { desc = "Find Files" })
        map("n", "<leader>fr", "<cmd>Telescope oldfiles<cr>", { desc = "Recent files" })
        map("n", "<leader>fb", "<cmd>Telescope buffers<cr>", { desc = "Buffers" })
        map("n", "<leader>fg", "<cmd>Telescope git_files<cr>", { desc = "Git files" })
        map("n", "<leader>fG", "<cmd>Telescope live_grep<cr>", { desc = "Live Grep" })
        map("n", "<leader>fw", "<cmd>Telescope grep_string<cr>", { desc = "Grep word" })
        map("n", "<leader>fh", "<cmd>Telescope help_tags<cr>", { desc = "Help Tags" })
        map("n", "<leader>fk", "<cmd>Telescope keymaps<cr>", { desc = "Keymaps" })
        map("n", "<leader>fc", "<cmd>Telescope commands<cr>", { desc = "Commands" })
        map("n", "<leader>fo", "<cmd>Telescope vim_options<cr>", { desc = "Options" })
        map("n", "<leader>fa", "<cmd>Telescope autocommands<cr>", { desc = "Autocommands" })
        map("n", "<leader>fs", "<cmd>Telescope lsp_document_symbols<cr>", { desc = "Document symbols" })
        map("n", "<leader>fS", "<cmd>Telescope lsp_dynamic_workspace_symbols<cr>", { desc = "Workspace symbols" })
        map("n", "<leader>fd", "<cmd>Telescope diagnostics bufnr=0<cr>", { desc = "Document diagnostics" })
        map("n", "<leader>fD", "<cmd>Telescope diagnostics<cr>", { desc = "Workspace diagnostics" })

        -- Git
        map("n", "<leader>gs", "<cmd>Git<cr>", { desc = "Git status" })
        map("n", "<leader>gb", "<cmd>Gitsigns blame_line<cr>", { desc = "Git Blame Line" })
        map("n", "<leader>gd", "<cmd>DiffviewOpen<cr>", { desc = "Git Diff View" })
        map("n", "<leader>gc", "<cmd>Git commit<cr>", { desc = "Git commit" })
        map("n", "<leader>gp", "<cmd>Git push<cr>", { desc = "Git push" })
        map("n", "<leader>gl", "<cmd>Gclog<cr>", { desc = "Git log" })
        map("n", "<leader>gf", "<cmd>Git fetch<cr>", { desc = "Git fetch" })

        -- Terminal
        map("n", "<leader>t", "<cmd>ToggleTerm<cr>", { desc = "Toggle Terminal" })

        -- Trouble
        map("n", "<leader>xx", "<cmd>TroubleToggle<cr>", { desc = "Toggle Trouble" })
        map("n", "<leader>xd", "<cmd>TroubleToggle document_diagnostics<cr>", { desc = "Document Diagnostics" })
        map("n", "<leader>xw", "<cmd>TroubleToggle workspace_diagnostics<cr>", { desc = "Workspace Diagnostics" })
        map("n", "<leader>xr", "<cmd>TroubleToggle lsp_references<cr>", { desc = "LSP References" })

        -- Flash (better f/F/t/T)
        map({ "n", "x", "o" }, "s", function() require("flash").jump() end, { desc = "Flash" })
        map({ "n", "x", "o" }, "S", function() require("flash").treesitter() end, { desc = "Flash Treesitter" })
        map({ "o", "x" }, "R", function() require("flash").treesitter_search() end, { desc = "Treesitter Search" })
        map({ "c" }, "<c-s>", function() require("flash").toggle() end, { desc = "Toggle Flash Search" })

        -- Undotree
        map("n", "<leader>u", vim.cmd.UndotreeToggle, { desc = "Undo Tree" })

        -- Lazy
        map("n", "<leader>l", "<cmd>Lazy<cr>", { desc = "Lazy" })

        -- Mason
        map("n", "<leader>cm", "<cmd>Mason<cr>", { desc = "Mason" })

        -- Spectre (search and replace)
        map("n", "<leader>sr", function() require("spectre").toggle() end, { desc = "Replace in files (Spectre)" })

        -- ========== AUTOCMDS (from config/autocmds.lua) ==========
        local function augroup(name)
          return vim.api.nvim_create_augroup("nvim_" .. name, { clear = true })
        end

        -- Check if we need to reload the file when it changed
        vim.api.nvim_create_autocmd({ "FocusGained", "TermClose", "TermLeave" }, {
          group = augroup("checktime"),
          command = "checktime",
        })

        -- Highlight on yank
        vim.api.nvim_create_autocmd("TextYankPost", {
          group = augroup("highlight_yank"),
          callback = function()
            vim.highlight.on_yank()
          end,
        })

        -- Resize splits if window got resized
        vim.api.nvim_create_autocmd({ "VimResized" }, {
          group = augroup("resize_splits"),
          callback = function()
            vim.cmd("tabdo wincmd =")
          end,
        })

        -- Go to last loc when opening a buffer
        vim.api.nvim_create_autocmd("BufReadPost", {
          group = augroup("last_loc"),
          callback = function()
            local exclude = { "gitcommit" }
            local buf = vim.api.nvim_get_current_buf()
            if vim.tbl_contains(exclude, vim.bo[buf].filetype) then
              return
            end
            local mark = vim.api.nvim_buf_get_mark(buf, '"')
            local lcount = vim.api.nvim_buf_line_count(buf)
            if mark[1] > 0 and mark[1] <= lcount then
              pcall(vim.api.nvim_win_set_cursor, 0, mark)
            end
          end,
        })

        -- Close some filetypes with <q>
        vim.api.nvim_create_autocmd("FileType", {
          group = augroup("close_with_q"),
          pattern = {
            "PlenaryTestPopup",
            "help",
            "lspinfo",
            "man",
            "notify",
            "qf",
            "spectre_panel",
            "startuptime",
            "tsplayground",
            "neotest-output",
            "checkhealth",
            "neotest-summary",
            "neotest-output-panel",
          },
          callback = function(event)
            vim.bo[event.buf].buflisted = false
            vim.keymap.set("n", "q", "<cmd>close<cr>", { buffer = event.buf, silent = true })
          end,
        })

        -- Wrap and check for spell in text filetypes
        vim.api.nvim_create_autocmd("FileType", {
          group = augroup("wrap_spell"),
          pattern = { "gitcommit", "markdown" },
          callback = function()
            vim.opt_local.wrap = true
            vim.opt_local.spell = true
          end,
        })

        -- Fix conceallevel for json files
        vim.api.nvim_create_autocmd({ "FileType" }, {
          group = augroup("nix_indent"),
          pattern = "nix",
          callback = function()
            vim.opt_local.shiftwidth = 2
            vim.opt_local.softtabstop = 2
          end,
        })

        -- Auto create dir when saving a file, in case some intermediate directory does not exist
        vim.api.nvim_create_autocmd({ "BufWritePre" }, {
          group = augroup("auto_create_dir"),
          callback = function(event)
            if event.match:match("^%w%w+://") then
              return
            end
            local file = vim.loop.fs_realpath(event.match) or event.match
            vim.fn.mkdir(vim.fn.fnamemodify(file, ":p:h"), "p")
          end,
        })

        -- Relative/absolute line number toggle
        vim.api.nvim_create_autocmd("InsertEnter", {
          group = augroup("relative_number_insert"),
          callback = function()
            vim.opt.relativenumber = false
          end,
        })

        vim.api.nvim_create_autocmd("InsertLeave", {
          group = augroup("relative_number_normal"),
          callback = function()
            vim.opt.relativenumber = true
          end,
        })

        -- Reset cursor shape when leaving Neovim
        vim.api.nvim_create_autocmd("VimLeave", {
          group = augroup("reset_cursor_shape"),
          callback = function()
            vim.opt.guicursor = ""
            vim.cmd("set guicursor=")
            vim.fn.chansend(vim.v.stderr, "\x1b[0 q")
          end,
        })

        -- ========== FILETYPE DETECTION (from ftdetect/) ==========
        -- Custom filetype detection rules
        vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
          group = augroup("custom_filetypes"),
          pattern = {
            ".jscsrc",
            ".jshintrc", 
            ".bowerrc",
            ".babelrc",
            ".eslintrc",
            ".tslintrc",
          },
          callback = function()
            vim.bo.filetype = "json"
          end,
        })

        vim.api.nvim_create_autocmd({ "BufRead", "BufNewFile" }, {
          group = augroup("tpl_filetype"),
          pattern = "*.tpl",
          callback = function()
            vim.bo.filetype = "html"
          end,
        })
      '';

      # Properly configured plugins with structured format
      plugins = [
        # ========== COLORSCHEMES ==========
        {
          plugin = pkgs.vimPlugins.tokyonight-nvim;
          type = "lua";
          config = ''
            local transparent = true
            require("tokyonight").setup({
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
            vim.cmd.colorscheme("tokyonight")
          '';
        }
        
        {
          plugin = pkgs.vimPlugins.catppuccin-nvim;
          type = "lua";
          config = ''
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
          '';
        }

        # ========== TREESITTER ==========
        {
          plugin = pkgs.vimPlugins.nvim-treesitter.withAllGrammars;
          type = "lua";
          config = ''
            local ok, treesitter_configs = pcall(require, "nvim-treesitter.configs")
            if ok then
              treesitter_configs.setup({
                highlight = { enable = true },
                indent = { enable = true },
                incremental_selection = {
                  enable = true,
                  keymaps = {
                    init_selection = "<C-space>",
                    node_incremental = "<C-space>",
                    scope_incremental = false,
                    node_decremental = "<bs>",
                  },
                },
                textobjects = {
                  select = {
                    enable = true,
                    lookahead = true,
                    keymaps = {
                      ["af"] = "@function.outer",
                      ["if"] = "@function.inner",
                      ["ac"] = "@class.outer",
                      ["ic"] = "@class.inner",
                      ["aa"] = "@parameter.outer",
                      ["ia"] = "@parameter.inner",
                      ["al"] = "@loop.outer",
                      ["il"] = "@loop.inner",
                      ["ai"] = "@conditional.outer",
                      ["ii"] = "@conditional.inner",
                      ["a/"] = "@comment.outer",
                      ["i/"] = "@comment.inner",
                      ["ab"] = "@block.outer",
                      ["ib"] = "@block.inner",
                    },
                  },
                  move = {
                    enable = true,
                    set_jumps = true,
                    goto_next_start = {
                      ["]f"] = "@function.outer",
                      ["]c"] = "@class.outer",
                      ["]a"] = "@parameter.inner",
                    },
                    goto_next_end = {
                      ["]F"] = "@function.outer",
                      ["]C"] = "@class.outer",
                      ["]A"] = "@parameter.inner",
                    },
                    goto_previous_start = {
                      ["[f"] = "@function.outer",
                      ["[c"] = "@class.outer",
                      ["[a"] = "@parameter.inner",
                    },
                    goto_previous_end = {
                      ["[F"] = "@function.outer",
                      ["[C"] = "@class.outer",
                      ["[A"] = "@parameter.inner",
                    },
                  },
                  swap = {
                    enable = true,
                    swap_next = {
                      ["<leader>a"] = "@parameter.inner",
                    },
                    swap_previous = {
                      ["<leader>A"] = "@parameter.inner",
                    },
                  },
                },
              })
            end
          '';
        }

        pkgs.vimPlugins.nvim-ts-context-commentstring
        
        {
          plugin = pkgs.vimPlugins.nvim-treesitter-context;
          type = "lua";
          config = ''
            local ok, treesitter_context = pcall(require, "treesitter-context")
            if ok then
              treesitter_context.setup({
                enable = true,
                max_lines = 0,
                min_window_height = 0,
                line_numbers = true,
                multiline_threshold = 20,
                trim_scope = 'outer',
                mode = 'cursor',
              })
            end
          '';
        }

        {
          plugin = pkgs.vimPlugins.nvim-ts-autotag;
          type = "lua";
          config = ''
            require("nvim-ts-autotag").setup()
          '';
        }

        # ========== LSP CONFIGURATION ==========
        {
          plugin = pkgs.vimPlugins.nvim-lspconfig;
          type = "lua";
          config = ''
            local lspconfig = require("lspconfig")
            local capabilities = require("cmp_nvim_lsp").default_capabilities()

            -- Enhanced diagnostics configuration
            vim.diagnostic.config({
              underline = true,
              update_in_insert = false,
              virtual_text = {
                spacing = 4,
                source = "if_many",
                prefix = "●",
              },
              severity_sort = true,
              signs = true,
              float = {
                border = "rounded",
                source = "always",
                header = "",
                prefix = "",
              },
            })

            -- LSP server configurations
            local servers = {
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
              tsserver = {
                settings = {
                  typescript = {
                    inlayHints = {
                      includeInlayParameterNameHints = "literal",
                      includeInlayParameterNameHintsWhenArgumentMatchesName = false,
                      includeInlayFunctionParameterTypeHints = true,
                      includeInlayVariableTypeHints = false,
                      includeInlayPropertyDeclarationTypeHints = true,
                      includeInlayFunctionLikeReturnTypeHints = true,
                      includeInlayEnumMemberValueHints = true,
                    },
                  },
                  javascript = {
                    inlayHints = {
                      includeInlayParameterNameHints = "all",
                      includeInlayParameterNameHintsWhenArgumentMatchesName = false,
                      includeInlayFunctionParameterTypeHints = true,
                      includeInlayVariableTypeHints = true,
                      includeInlayPropertyDeclarationTypeHints = true,
                      includeInlayFunctionLikeReturnTypeHints = true,
                      includeInlayEnumMemberValueHints = true,
                    },
                  },
                },
              },
              pyright = {
                settings = {
                  python = {
                    analysis = {
                      typeCheckingMode = "off",
                    },
                  },
                },
              },
              gopls = {
                settings = {
                  gopls = {
                    gofumpt = true,
                    codelenses = {
                      gc_details = false,
                      generate = true,
                      regenerate_cgo = true,
                      run_govulncheck = true,
                      test = true,
                      tidy = true,
                      upgrade_dependency = true,
                      vendor = true,
                    },
                    hints = {
                      assignVariableTypes = true,
                      compositeLiteralFields = true,
                      compositeLiteralTypes = true,
                      constantValues = true,
                      functionTypeParameters = true,
                      parameterNames = true,
                      rangeVariableTypes = true,
                    },
                    analyses = {
                      fieldalignment = true,
                      nilness = true,
                      unusedparams = true,
                      unusedwrite = true,
                      useany = true,
                    },
                    usePlaceholders = true,
                    completeUnimported = true,
                    staticcheck = true,
                    directoryFilters = { "-.git", "-.vscode", "-.idea", "-.vscode-test", "-node_modules" },
                    semanticTokens = true,
                  },
                },
              },
              nil_ls = {
                settings = {
                  ["nil"] = {
                    testSetting = 42,
                    formatting = {
                      command = { "nixfmt" },
                    },
                  },
                },
              },
            }

            -- Set up each server
            for server, config in pairs(servers) do
              config.capabilities = capabilities
              config.on_attach = function(client, bufnr)
                local opts = { buffer = bufnr, silent = true }
                
                -- LSP keymaps
                vim.keymap.set("n", "gd", vim.lsp.buf.definition, vim.tbl_extend("force", opts, { desc = "Go to definition" }))
                vim.keymap.set("n", "gD", vim.lsp.buf.declaration, vim.tbl_extend("force", opts, { desc = "Go to declaration" }))
                vim.keymap.set("n", "gr", vim.lsp.buf.references, vim.tbl_extend("force", opts, { desc = "References" }))
                vim.keymap.set("n", "gi", vim.lsp.buf.implementation, vim.tbl_extend("force", opts, { desc = "Go to implementation" }))
                vim.keymap.set("n", "gt", vim.lsp.buf.type_definition, vim.tbl_extend("force", opts, { desc = "Type definition" }))
                vim.keymap.set("n", "K", vim.lsp.buf.hover, vim.tbl_extend("force", opts, { desc = "Hover" }))
                vim.keymap.set("n", "<leader>sh", vim.lsp.buf.signature_help, vim.tbl_extend("force", opts, { desc = "Signature Help" }))
                vim.keymap.set("i", "<leader>sh", vim.lsp.buf.signature_help, vim.tbl_extend("force", opts, { desc = "Signature Help" }))
                vim.keymap.set("n", "<leader>rn", vim.lsp.buf.rename, vim.tbl_extend("force", opts, { desc = "Rename" }))
                vim.keymap.set({ "n", "v" }, "<leader>ca", vim.lsp.buf.code_action, vim.tbl_extend("force", opts, { desc = "Code Action" }))
                vim.keymap.set("n", "<leader>f", function()
                  vim.lsp.buf.format({ async = true })
                end, vim.tbl_extend("force", opts, { desc = "Format" }))
                
                -- Diagnostics
                vim.keymap.set("n", "<leader>cd", vim.diagnostic.open_float, vim.tbl_extend("force", opts, { desc = "Line Diagnostics" }))
                vim.keymap.set("n", "[d", vim.diagnostic.goto_prev, vim.tbl_extend("force", opts, { desc = "Previous Diagnostic" }))
                vim.keymap.set("n", "]d", vim.diagnostic.goto_next, vim.tbl_extend("force", opts, { desc = "Next Diagnostic" }))

                -- Document highlight
                if client.server_capabilities.documentHighlightProvider then
                  vim.api.nvim_create_autocmd({ "CursorHold", "CursorHoldI" }, {
                    buffer = bufnr,
                    callback = vim.lsp.buf.document_highlight,
                  })
                  vim.api.nvim_create_autocmd({ "CursorMoved", "CursorMovedI" }, {
                    buffer = bufnr,
                    callback = vim.lsp.buf.clear_references,
                  })
                end

                -- Inlay hints (if supported)
                if client.server_capabilities.inlayHintProvider then
                  vim.lsp.inlay_hint.enable(true, { bufnr = bufnr })
                end
              end
              lspconfig[server].setup(config)
            end
          '';
        }

        # ========== COMPLETION ==========
        {
          plugin = pkgs.vimPlugins.nvim-cmp;
          type = "lua";
          config = ''
            local cmp = require("cmp")
            local luasnip = require("luasnip")

            cmp.setup({
              completion = {
                completeopt = "menu,menuone,noinsert",
              },
              snippet = {
                expand = function(args)
                  luasnip.lsp_expand(args.body)
                end,
              },
              window = {
                completion = cmp.config.window.bordered(),
                documentation = cmp.config.window.bordered(),
              },
              mapping = cmp.mapping.preset.insert({
                ["<C-n>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
                ["<C-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
                ["<C-b>"] = cmp.mapping.scroll_docs(-4),
                ["<C-f>"] = cmp.mapping.scroll_docs(4),
                ["<C-Space>"] = cmp.mapping.complete(),
                ["<C-e>"] = cmp.mapping.abort(),
                ["<CR>"] = cmp.mapping.confirm({ 
                  behavior = cmp.ConfirmBehavior.Replace,
                  select = true 
                }),
                ["<S-CR>"] = cmp.mapping.confirm({
                  behavior = cmp.ConfirmBehavior.Replace,
                  select = true,
                }),
                ["<Tab>"] = cmp.mapping(function(fallback)
                  if cmp.visible() then
                    cmp.select_next_item()
                  elseif luasnip.expand_or_locally_jumpable() then
                    luasnip.expand_or_jump()
                  else
                    fallback()
                  end
                end, { "i", "s" }),
                ["<S-Tab>"] = cmp.mapping(function(fallback)
                  if cmp.visible() then
                    cmp.select_prev_item()
                  elseif luasnip.locally_jumpable(-1) then
                    luasnip.jump(-1)
                  else
                    fallback()
                  end
                end, { "i", "s" }),
              }),
              sources = cmp.config.sources({
                { name = "copilot", priority = 1000 },
                { name = "nvim_lsp", priority = 1000 },
                { name = "luasnip", priority = 750 },
                { name = "nvim_lua", priority = 500 },
              }, {
                { name = "buffer", priority = 250, keyword_length = 3 },
                { name = "path", priority = 250 },
              }),
              formatting = {
                expandable_indicator = true,
                fields = { "kind", "abbr", "menu" },
                format = function(entry, vim_item)
                  local kind_icons = {
                    Text = "",
                    Method = "󰆧",
                    Function = "󰊕",
                    Constructor = "",
                    Field = "󰇽",
                    Variable = "󰂡",
                    Class = "󰠱",
                    Interface = "",
                    Module = "",
                    Property = "󰜢",
                    Unit = "",
                    Value = "󰎠",
                    Enum = "",
                    Keyword = "󰌋",
                    Snippet = "",
                    Color = "󰏘",
                    File = "󰈙",
                    Reference = "",
                    Folder = "󰉋",
                    EnumMember = "",
                    Constant = "󰏿",
                    Struct = "",
                    Event = "",
                    Operator = "󰆕",
                    TypeParameter = "󰅲",
                  }
                  vim_item.kind = string.format('%s %s', kind_icons[vim_item.kind], vim_item.kind)
                  vim_item.menu = ({
                    nvim_lsp = "[LSP]",
                    nvim_lua = "[Lua]",
                    luasnip = "[LuaSnip]",
                    buffer = "[Buffer]",
                    path = "[Path]",
                  })[entry.source.name]
                  return vim_item
                end,
              },
            })

            -- Set configuration for specific filetype
            cmp.setup.filetype("gitcommit", {
              sources = cmp.config.sources({
                { name = "buffer" },
              })
            })

            -- Use buffer source for `/` and `?`
            cmp.setup.cmdline({ "/", "?" }, {
              mapping = cmp.mapping.preset.cmdline(),
              sources = {
                { name = "buffer" }
              }
            })

            -- Use cmdline & path source for ':'
            cmp.setup.cmdline(":", {
              mapping = cmp.mapping.preset.cmdline(),
              sources = cmp.config.sources({
                { name = "path" }
              }, {
                { name = "cmdline" }
              })
            })
          '';
        }

        pkgs.vimPlugins.cmp-nvim-lsp
        pkgs.vimPlugins.cmp-buffer
        pkgs.vimPlugins.cmp-path
        pkgs.vimPlugins.cmp-cmdline
        pkgs.vimPlugins.cmp_luasnip
        pkgs.vimPlugins.cmp-nvim-lua

        {
          plugin = pkgs.vimPlugins.luasnip;
          type = "lua";
          config = ''
            require("luasnip.loaders.from_vscode").lazy_load()
          '';
        }

        pkgs.vimPlugins.friendly-snippets

        # Mason for LSP management
        {
          plugin = pkgs.vimPlugins.mason-nvim;
          type = "lua";
          config = ''
            require("mason").setup()
          '';
        }

        {
          plugin = pkgs.vimPlugins.mason-lspconfig-nvim;
          type = "lua";
          config = ''
            require("mason-lspconfig").setup({
              ensure_installed = { "lua_ls", "tsserver", "pyright", "gopls" },
            })
          '';
        }

        {
          plugin = pkgs.vimPlugins.mason-tool-installer-nvim;
          type = "lua";
          config = ''
            require("mason-tool-installer").setup({
              ensure_installed = {
                "prettier",
                "stylua",
                "black",
              },
            })
          '';
        }

        {
          plugin = pkgs.vimPlugins.fidget-nvim;
          type = "lua";
          config = ''
            require("fidget").setup()
          '';
        }

        # {
        #   plugin = pkgs.vimPlugins.neodev-nvim;
        #   type = "lua";
        #   config = ''
        #     require("neodev").setup()
        #   '';
        # }

        # Formatting and linting
        {
          plugin = pkgs.vimPlugins.conform-nvim;
          type = "lua";
          config = ''
            require("conform").setup({
              formatters_by_ft = {
                lua = { "stylua" },
                python = { "black" },
                javascript = { "prettier" },
                typescript = { "prettier" },
                json = { "prettier" },
                yaml = { "prettier" },
                markdown = { "prettier" },
                nix = { "nixfmt" },
              },
              format_on_save = {
                timeout_ms = 500,
                lsp_fallback = true,
              },
            })
          '';
        }

        {
          plugin = pkgs.vimPlugins.nvim-lint;
          type = "lua";
          config = ''
            require("lint").linters_by_ft = {
              python = { "flake8" },
              javascript = { "eslint" },
              typescript = { "eslint" },
            }

            vim.api.nvim_create_autocmd({ "BufWritePost" }, {
              callback = function()
                require("lint").try_lint()
              end,
            })
          '';
        }

        # Editor enhancements
        {
          plugin = pkgs.vimPlugins.mini-pairs;
          type = "lua";
          config = ''
            require("mini.pairs").setup()
          '';
        }

        {
          plugin = pkgs.vimPlugins.mini-surround;
          type = "lua";
          config = ''
            require("mini.surround").setup()
          '';
        }

        {
          plugin = pkgs.vimPlugins.mini-comment;
          type = "lua";
          config = ''
            require("mini.comment").setup()
          '';
        }

        {
          plugin = pkgs.vimPlugins.mini-ai;
          type = "lua";
          config = ''
            require("mini.ai").setup()
          '';
        }

        {
          plugin = pkgs.vimPlugins.mini-indentscope;
          type = "lua";
          config = ''
            require("mini.indentscope").setup({
              symbol = "│",
              options = { try_as_border = true },
            })
          '';
        }

        # ========== UI IMPROVEMENTS ==========
        {
          plugin = pkgs.vimPlugins.dressing-nvim;
          type = "lua";
          config = ''
            require("dressing").setup({
              input = {
                enabled = true,
                default_prompt = "➤ ",
                win_options = {
                  winblend = 0,
                },
              },
              select = {
                enabled = true,
                backend = { "telescope", "builtin" },
              },
            })
          '';
        }

        # Commented out due to hash mismatch build error
        # {
        #   plugin = pkgs.vimPlugins.lualine-nvim;
        #   type = "lua";
        #   config = ''
        #     require("lualine").setup({
        #       options = {
        #         theme = "tokyonight",
        #         component_separators = { left = "", right = "" },
        #         section_separators = { left = "", right = "" },
        #         globalstatus = true,
        #         disabled_filetypes = { statusline = { "dashboard", "alpha" } },
        #       },
        #       sections = {
        #         lualine_a = { "mode" },
        #         lualine_b = { "branch", "diff", "diagnostics" },
        #         lualine_c = {
        #           {
        #             "filename",
        #             path = 1,
        #             symbols = {
        #               modified = "  ",
        #               readonly = "",
        #               unnamed = "",
        #             },
        #           },
        #         },
        #         lualine_x = {
        #           {
        #             function()
        #               local msg = "No Active Lsp"
        #               local buf_ft = vim.api.nvim_buf_get_option(0, "filetype")
        #               local clients = vim.lsp.get_clients()
        #               if next(clients) == nil then
        #                 return msg
        #               end
        #               for _, client in ipairs(clients) do
        #                 local filetypes = client.config.filetypes
        #                 if filetypes and vim.fn.index(filetypes, buf_ft) ~= -1 then
        #                   return client.name
        #                 end
        #               end
        #               return msg
        #             end,
        #             icon = " ",
        #             color = { fg = "#ffffff", gui = "bold" },
        #           },
        #           "encoding",
        #           "fileformat",
        #           "filetype",
        #         },
        #         lualine_y = { "progress" },
        #         lualine_z = { "location" },
        #       },
        #       extensions = { "nvim-tree", "lazy" },
        #     })
        #   '';
        # }

        pkgs.vimPlugins.nvim-web-devicons

        {
          plugin = pkgs.vimPlugins.indent-blankline-nvim;
          type = "lua";
          config = ''
            require("ibl").setup({
              indent = {
                char = "│",
                tab_char = "│",
              },
              scope = { enabled = false },
              exclude = {
                filetypes = {
                  "help",
                  "alpha",
                  "dashboard",
                  "Trouble",
                  "lazy",
                  "mason",
                  "notify",
                  "toggleterm",
                  "lazyterm",
                },
              },
            })
          '';
        }

        {
          plugin = pkgs.vimPlugins.alpha-nvim;
          type = "lua";
          config = ''
            local alpha = require("alpha")
            local dashboard = require("alpha.themes.dashboard")

            dashboard.section.header.val = {
              "                                                     ",
              "  ███╗   ██╗███████╗ ██████╗ ██╗   ██╗██╗███╗   ███╗ ",
              "  ████╗  ██║██╔════╝██╔═══██╗██║   ██║██║████╗ ████║ ",
              "  ██╔██╗ ██║█████╗  ██║   ██║██║   ██║██║██╔████╔██║ ",
              "  ██║╚██╗██║██╔══╝  ██║   ██║╚██╗ ██╔╝██║██║╚██╔╝██║ ",
              "  ██║ ╚████║███████╗╚██████╔╝ ╚████╔╝ ██║██║ ╚═╝ ██║ ",
              "  ╚═╝  ╚═══╝╚══════╝ ╚═════╝   ╚═══╝  ╚═╝╚═╝     ╚═╝ ",
              "                                                     ",
            }

            dashboard.section.buttons.val = {
              dashboard.button("f", " " .. " Find file", ":Telescope find_files <CR>"),
              dashboard.button("n", " " .. " New file", ":ene <BAR> startinsert <CR>"),
              dashboard.button("r", " " .. " Recent files", ":Telescope oldfiles <CR>"),
              dashboard.button("g", " " .. " Find text", ":Telescope live_grep <CR>"),
              dashboard.button("c", " " .. " Config", ":e $MYVIMRC <CR>"),
              dashboard.button("q", " " .. " Quit", ":qa<CR>"),
            }

            alpha.setup(dashboard.config)
          '';
        }

        {
          plugin = pkgs.vimPlugins.nvim-notify;
          type = "lua";
          config = ''
            require("notify").setup({
              background_colour = "#000000",
            })
            vim.notify = require("notify")
          '';
        }

        {
          plugin = pkgs.vimPlugins.noice-nvim;
          type = "lua";
          config = ''
            require("noice").setup({
              lsp = {
                override = {
                  ["vim.lsp.util.convert_input_to_markdown_lines"] = true,
                  ["vim.lsp.util.stylize_markdown"] = true,
                  ["cmp.entry.get_documentation"] = true,
                },
              },
              presets = {
                bottom_search = true,
                command_palette = true,
                long_message_to_split = true,
                inc_rename = false,
                lsp_doc_border = false,
              },
            })
          '';
        }

        pkgs.vimPlugins.nui-nvim

        # ========== FILE MANAGEMENT ==========
        {
          plugin = pkgs.vimPlugins.nvim-tree-lua;
          type = "lua";
          config = ''
            require("nvim-tree").setup({
              disable_netrw = true,
              hijack_netrw = true,
              respect_buf_cwd = true,
              sync_root_with_cwd = true,
              view = {
                width = 30,
                side = "left",
              },
              renderer = {
                group_empty = true,
                highlight_git = true,
                icons = {
                  show = {
                    file = true,
                    folder = true,
                    folder_arrow = true,
                    git = true,
                  },
                },
              },
              filters = {
                dotfiles = false,
              },
              git = {
                enable = true,
                ignore = false,
              },
            })
          '';
        }

        {
          plugin = pkgs.vimPlugins.telescope-nvim;
          type = "lua";
          config = ''
            local telescope = require("telescope")
            local actions = require("telescope.actions")

            telescope.setup({
              defaults = {
                prompt_prefix = " ",
                selection_caret = " ",
                mappings = {
                  i = {
                    ["<C-n>"] = actions.move_selection_next,
                    ["<C-p>"] = actions.move_selection_previous,
                    ["<C-j>"] = actions.move_selection_next,
                    ["<C-k>"] = actions.move_selection_previous,
                    ["<C-u>"] = actions.preview_scrolling_up,
                    ["<C-d>"] = actions.preview_scrolling_down,
                    ["<esc>"] = actions.close,
                  },
                },
              },
              pickers = {
                find_files = {
                  theme = "dropdown",
                  previewer = false,
                  hidden = true,
                },
                buffers = {
                  theme = "dropdown",
                  previewer = false,
                  initial_mode = "normal",
                },
              },
              extensions = {
                fzf = {
                  fuzzy = true,
                  override_generic_sorter = true,
                  override_file_sorter = true,
                  case_mode = "smart_case",
                },
              },
            })

            telescope.load_extension("fzf")
          '';
        }

        {
          plugin = pkgs.vimPlugins.telescope-fzf-native-nvim;
          type = "lua";
          config = ''
          '';
        }

        pkgs.vimPlugins.plenary-nvim

        # ========== GIT INTEGRATION ==========
        {
          plugin = pkgs.vimPlugins.gitsigns-nvim;
          type = "lua";
          config = ''
            require("gitsigns").setup({
              signs = {
                add = { text = "▎" },
                change = { text = "▎" },
                delete = { text = "" },
                topdelete = { text = "" },
                changedelete = { text = "▎" },
                untracked = { text = "▎" },
              },
              on_attach = function(buffer)
                local gs = package.loaded.gitsigns
                local function map(mode, l, r, desc)
                  vim.keymap.set(mode, l, r, { buffer = buffer, desc = desc })
                end

                -- Navigation
                map("n", "]h", gs.next_hunk, "Next Hunk")
                map("n", "[h", gs.prev_hunk, "Prev Hunk")

                -- Actions
                map({ "n", "v" }, "<leader>ghs", ":Gitsigns stage_hunk<CR>", "Stage Hunk")
                map({ "n", "v" }, "<leader>ghr", ":Gitsigns reset_hunk<CR>", "Reset Hunk")
                map("n", "<leader>ghS", gs.stage_buffer, "Stage Buffer")
                map("n", "<leader>ghu", gs.undo_stage_hunk, "Undo Stage Hunk")
                map("n", "<leader>ghR", gs.reset_buffer, "Reset Buffer")
                map("n", "<leader>ghp", gs.preview_hunk, "Preview Hunk")
                map("n", "<leader>ghb", function()
                  gs.blame_line({ full = true })
                end, "Blame Line")
                map("n", "<leader>ghd", gs.diffthis, "Diff This")
                map("n", "<leader>ghD", function()
                  gs.diffthis("~")
                end, "Diff This ~")

                -- Text object
                map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>", "GitSigns Select Hunk")
              end,
            })
          '';
        }

        pkgs.vimPlugins.vim-fugitive
        pkgs.vimPlugins.vim-rhubarb

        {
          plugin = pkgs.vimPlugins.diffview-nvim;
          type = "lua";
          config = ''
            require("diffview").setup()
          '';
        }

        {
          plugin = pkgs.vimPlugins.git-blame-nvim;
          type = "lua";
          config = ''
            vim.g.gitblame_enabled = 0
          '';
        }

        # Utilities
        {
          plugin = pkgs.vimPlugins.which-key-nvim;
          type = "lua";
          config = ''
            require("which-key").setup({
              plugins = {
                marks = true,
                registers = true,
                spelling = {
                  enabled = true,
                  suggestions = 20,
                },
              },
            })
          '';
        }

        {
          plugin = pkgs.vimPlugins.persistence-nvim;
          type = "lua";
          config = ''
            require("persistence").setup()
          '';
        }

        {
          plugin = pkgs.vimPlugins.trouble-nvim;
          type = "lua";
          config = ''
            require("trouble").setup()
          '';
        }

        {
          plugin = pkgs.vimPlugins.todo-comments-nvim;
          type = "lua";
          config = ''
            require("todo-comments").setup()
          '';
        }

        {
          plugin = pkgs.vimPlugins.nvim-colorizer-lua;
          type = "lua";
          config = ''
            require("colorizer").setup()
          '';
        }

        {
          plugin = pkgs.vimPlugins.vim-illuminate;
          type = "lua";
          config = ''
            require("illuminate").configure({
              providers = {
                "lsp",
                "treesitter",
                "regex",
              },
              delay = 120,
              filetype_overrides = {},
              filetypes_denylist = {
                "dirvish",
                "fugitive",
                "alpha",
                "NvimTree",
                "lazy",
                "neogitstatus",
                "Trouble",
                "lir",
                "Outline",
                "spectre_panel",
                "toggleterm",
                "DressingSelect",
                "TelescopePrompt",
              },
            })
          '';
        }

        {
          plugin = pkgs.vimPlugins.flash-nvim;
          type = "lua";
          config = ''
            require("flash").setup({
              modes = {
                search = {
                  enabled = false,
                },
                char = {
                  enabled = true,
                  jump_labels = true,
                },
              },
            })
          '';
        }

        {
          plugin = pkgs.vimPlugins.nvim-spectre;
          type = "lua";
          config = ''
            require("spectre").setup()
          '';
        }

        pkgs.vimPlugins.undotree
        pkgs.vimPlugins.vim-repeat

        {
          plugin = pkgs.vimPlugins.nvim-surround;
          type = "lua";
          config = ''
            require("nvim-surround").setup()
          '';
        }

        {
          plugin = pkgs.vimPlugins.comment-nvim;
          type = "lua";
          config = ''
            require("Comment").setup()
          '';
        }

        {
          plugin = pkgs.vimPlugins.nvim-bqf;
          type = "lua";
          config = ''
            require("bqf").setup()
          '';
        }

        {
          plugin = pkgs.vimPlugins.copilot-lua;
          type = "lua";
          config = ''
            require("copilot").setup({
              suggestion = {
                auto_trigger = true,
                keymap = {
                  accept = "<M-l>",
                  dismiss = "<M-h>",
                },
              },
              panel = {
                enabled = true,
                keymap = {
                  jump_prev = "[[",
                  jump_next = "]]",
                  accept = "<cr>",
                  refresh = "gr",
                  open = "<C-c>p",
                },
              },
              filetypes = {
                markdown = true,
                help = true,
              },
            })
          '';
        }

        pkgs.vimPlugins.copilot-cmp

        # Terminal integration
        {
          plugin = pkgs.vimPlugins.toggleterm-nvim;
          type = "lua";
          config = ''
            require("toggleterm").setup({
              size = 20,
              open_mapping = [[<c-\>]],
              hide_numbers = true,
              shade_terminals = true,
              shading_factor = 2,
              start_in_insert = true,
              insert_mappings = true,
              persist_size = true,
              direction = "float",
              close_on_exit = true,
              shell = vim.o.shell,
              float_opts = {
                border = "curved",
              },
            })
          '';
        }

        # Development tools
        pkgs.vimPlugins.vim-wakatime
        pkgs.vimPlugins.vim-startuptime

        {
          plugin = pkgs.vimPlugins.treesj;
          type = "lua";
          config = ''
            require("treesj").setup({
              use_default_keymaps = false,
            })

            vim.keymap.set("n", "<leader>j", require("treesj").toggle, { desc = "Toggle Split/Join" })
          '';
        }

        # GPG Integration
        {
          plugin = pkgs.vimUtils.buildVimPlugin {
            pname = "gpg-nvim";
            version = "2024-08-28";
            src = pkgs.fetchFromGitHub {
              owner = "benoror";
              repo = "gpg.nvim";
              rev = "92c770089ce0fdc3bd5facd9c61c45654e90688e";
              hash = "sha256-ZDwgiWuhqixoVJo1CFHoMv7SAVZjnjYPtk+tf1b/2k4=";
            };
          };
          type = "lua";
          config = ''
            -- gpg.nvim plugin for transparent editing of GPG encrypted files
            -- The plugin automatically sets up autocmds for *.gpg files
            -- No additional configuration needed as it works out of the box
            -- 
            -- Features:
            -- - Automatically decrypts *.gpg files when opened
            -- - Automatically encrypts on save
            -- - Disables swap files, undo files, and shada for security
            -- - Uses symmetric encryption by default (gpg -ae)
          '';
        }
      ];

      # Ensure we have the necessary runtime dependencies
      extraPackages = with pkgs; [
        # Language servers and tools
        lua-language-server
        nodePackages.typescript-language-server
        nodePackages.vscode-langservers-extracted
        pyright
        gopls
        nil # Nix language server

        # Formatters and linters
        stylua
        nodePackages.prettier
        black
        rustfmt
        nixfmt

        # Tools used by plugins
        ripgrep
        fd
        fzf
        tree-sitter
        gnupg # Required for gpg.nvim plugin

        # Clipboard support
        xclip # for x11
      ] ++ lib.optionals pkgs.stdenv.isLinux [
        wl-clipboard # for wayland (Linux only)
      ];
    };
  };
}
