return {
  -- Git integration
  {
    "tpope/vim-fugitive",
    cmd = {
      "Git",
      "GBrowse",
      "Gdiffsplit",
      "Gvdiffsplit",
      "Gedit",
      "Gsplit",
      "Gread",
      "Gwrite",
      "Ggrep",
      "Glgrep",
      "Gmove",
      "Gdelete",
      "Gremove",
      "Gbrowse",
      "Gblame",
      "Gclog",
      "Glog",
    },
    keys = {
      { "<leader>gs", "<cmd>Git<cr>", desc = "Git status" },
      { "<leader>gb", "<cmd>Git blame<cr>", desc = "Git blame" },
      { "<leader>gd", "<cmd>Gvdiffsplit<cr>", desc = "Git diff split" },
      { "<leader>gc", "<cmd>Git commit<cr>", desc = "Git commit" },
      { "<leader>gp", "<cmd>Git push<cr>", desc = "Git push" },
      { "<leader>gl", "<cmd>Gclog<cr>", desc = "Git log" },
      { "<leader>gf", "<cmd>Git fetch<cr>", desc = "Git fetch" },
    },
  },

  -- Git signs
  {
    "lewis6991/gitsigns.nvim",
    event = { "BufReadPre", "BufNewFile" },
    opts = {
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
    },
  },

  -- GitHub integration
  {
    "tpope/vim-rhubarb",
    dependencies = "tpope/vim-fugitive",
    cmd = { "GBrowse" },
  },

  -- Better diff
  {
    "sindrets/diffview.nvim",
    cmd = { "DiffviewOpen", "DiffviewClose", "DiffviewToggleFiles", "DiffviewFocusFiles" },
    config = true,
    keys = {
      { "<leader>gD", "<cmd>DiffviewOpen<cr>", desc = "DiffView" },
    },
  },

  -- Git blame
  {
    "f-person/git-blame.nvim",
    event = { "BufReadPre", "BufNewFile" },
    opts = {
      enabled = false,
      message_template = " <summary> • <date> • <author> • <sha>",
      date_format = "%m-%d-%Y %H:%M:%S",
      virtual_text_column = 1,
    },
    keys = {
      { "<leader>gtb", "<cmd>GitBlameToggle<cr>", desc = "Toggle git blame" },
      { "<leader>gts", "<cmd>GitBlameCopySHA<cr>", desc = "Copy commit SHA" },
      { "<leader>gtc", "<cmd>GitBlameCopyCommitURL<cr>", desc = "Copy commit URL" },
      { "<leader>gtf", "<cmd>GitBlameCopyFileURL<cr>", desc = "Copy file URL" },
    },
  },
}