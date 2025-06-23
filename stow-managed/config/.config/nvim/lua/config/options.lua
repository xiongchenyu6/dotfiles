local opt = vim.opt

-- UI
opt.number = true
opt.relativenumber = true
opt.termguicolors = true
opt.signcolumn = "yes"
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
opt.listchars = { tab = "» ", trail = "·", nbsp = "␣" }
opt.fillchars = {
  vert = "│",
  fold = "⠀",
  eob = " ",
  msgsep = "‾",
  foldopen = "▾",
  foldsep = "│",
  foldclose = "▸",
}

-- Editing
opt.expandtab = true
opt.shiftwidth = 2
opt.tabstop = 2
opt.softtabstop = 2
opt.smartindent = true
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
opt.splitkeep = "screen"

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
opt.spelllang = { "en" }
opt.completeopt = { "menuone", "noselect", "noinsert" }
opt.wildmode = "longest:full,full"
opt.shortmess:append({ W = true, I = true, c = true })
opt.iskeyword:append("-")
opt.formatoptions:remove({ "c", "r", "o" })
opt.runtimepath:remove("/usr/share/vim/vimfiles")

-- Disable nvim intro
opt.shortmess:append("sI")

-- go to previous/next line with h,l,left arrow and right arrow
-- when cursor reaches end/beginning of line
opt.whichwrap:append("<>[]hl")

-- Disable some builtin providers
vim.g.loaded_node_provider = 0
vim.g.loaded_python3_provider = 0
vim.g.loaded_perl_provider = 0
vim.g.loaded_ruby_provider = 0

-- Fix markdown indentation settings
vim.g.markdown_recommended_style = 0