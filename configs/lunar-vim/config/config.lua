--[[
lvim is the global options object

Linters should be
filled in as strings with either
a global executable or a path to
an executable
]]
-- THESE ARE EXAMPLE CONFIGS FEEL FREE TO CHANGE TO WHATEVER YOU WANT

-- general
lvim.log.level = "warn"
lvim.format_on_save = true
lvim.colorscheme = "onedarker"

vim.opt.cmdheight = 1
vim.opt.timeoutlen = 500
vim.opt.clipboard = ""

-- Mapping the semicolon to colon in all relevant modes.
vim.api.nvim_set_keymap("n", ";", ":", { noremap = true, silent = false })
vim.api.nvim_set_keymap("v", ";", ":", { noremap = true, silent = false })

-- Navigate through wrapped long lines intuitively.
vim.api.nvim_set_keymap("n", "j", "gj", { noremap = true, silent = false })
vim.api.nvim_set_keymap("v", "k", "gk", { noremap = true, silent = false })

-- Motion in insert mode
vim.api.nvim_set_keymap("i", "<C-h>", "<left>", { noremap = true, silent = false })
vim.api.nvim_set_keymap("i", "<C-l>", "<right>", { noremap = true, silent = false })
vim.api.nvim_set_keymap("i", "<C-j>", "<down>", { noremap = true, silent = false })
vim.api.nvim_set_keymap("i", "<C-k>", "<up>", { noremap = true, silent = false })

-- Motion in command mode
vim.api.nvim_set_keymap("c", "<C-h>", "<left>", { noremap = true, silent = false })
vim.api.nvim_set_keymap("c", "<C-l>", "<right>", { noremap = true, silent = false })
vim.api.nvim_set_keymap("c", "<C-j>", "<down>", { noremap = true, silent = false })
vim.api.nvim_set_keymap("c", "<C-k>", "<up>", { noremap = true, silent = false })

-- Clipboard handling
vim.api.nvim_set_keymap("v", "<C-c>", '"*y :let @+=@*<cr>', { noremap = true, silent = false })
vim.api.nvim_set_keymap("n", "<C-p>", '"+p', { noremap = true, silent = false })

-- Selection
vim.api.nvim_set_keymap("n", "<cr>", '*``', { noremap = true, silent = true })

lvim.line_wrap_cursor_movement = false

-- keymappings [view all the defaults by pressing <leader>Lk]
lvim.leader = "space"
-- add your own keymapping
lvim.keys.normal_mode["<C-s>"] = ":w<cr>"
-- unmap a default keymapping
-- lvim.keys.normal_mode["<C-Up>"] = ""
-- edit a default keymapping
-- lvim.keys.normal_mode["<C-q>"] = ":q<cr>"

-- Change Telescope navigation to use j and k for navigation and n and p for history in both input and normal mode.
lvim.builtin.telescope.on_config_done = function()
  local actions = require "telescope.actions"
  -- for input mode
  lvim.builtin.telescope.defaults.mappings.i["<C-j>"] = actions.move_selection_next
  lvim.builtin.telescope.defaults.mappings.i["<C-k>"] = actions.move_selection_previous
  lvim.builtin.telescope.defaults.mappings.i["<C-n>"] = actions.cycle_previewers_next
  lvim.builtin.telescope.defaults.mappings.i["<C-p>"] = actions.cycle_previewers_prev
  lvim.builtin.telescope.defaults.mappings.i["<C-v>"] = actions.select_vertical
  lvim.builtin.telescope.defaults.mappings.i["<C-x>"] = actions.select_horizontal
  lvim.builtin.telescope.defaults.mappings.i["<C-t>"] = actions.select_tab
  -- for normal mode
  lvim.builtin.telescope.defaults.mappings.n["<C-j>"] = actions.move_selection_next
  lvim.builtin.telescope.defaults.mappings.n["<C-k>"] = actions.move_selection_previous
end

-- Use which-key to add extra bindings with the leader-key prefix
-- lvim.builtin.which_key.mappings["P"] = { "<cmd>Telescope projects<CR>", "Projects" }
-- lvim.builtin.which_key.mappings["t"] = {
--   name = "+Trouble",
--   r = { "<cmd>Trouble lsp_references<cr>", "References" },
--   f = { "<cmd>Trouble lsp_definitions<cr>", "Definitions" },
--   d = { "<cmd>Trouble lsp_document_diagnostics<cr>", "Diagnostics" },
--   q = { "<cmd>Trouble quickfix<cr>", "QuickFix" },
--   l = { "<cmd>Trouble loclist<cr>", "LocationList" },
--   w = { "<cmd>Trouble lsp_workspace_diagnostics<cr>", "Diagnostics" },
-- }
lvim.builtin.which_key.mappings["t"] = {
  name = "+Toggle",
  w = { "<cmd>setlocal wrap!<cr>", "Line Wrapping" },
}

-- TODO: User Config for predefined plugins
-- After changing plugin config exit and reopen LunarVim, Run :PackerInstall :PackerCompile
lvim.builtin.dashboard.active = true
lvim.builtin.terminal.active = true
lvim.builtin.nvimtree.side = "left"
lvim.builtin.nvimtree.show_icons.git = 0

-- if you don't want all the parsers change this to a table of the ones you want
lvim.builtin.treesitter.ensure_installed = "maintained"
lvim.builtin.treesitter.ignore_install = { "haskell" }
lvim.builtin.treesitter.highlight.enabled = true

-- generic LSP settings
-- you can set a custom on_attach function that will be used for all the language servers
-- See <https://github.com/neovim/nvim-lspconfig#keybindings-and-completion>
-- lvim.lsp.on_attach_callback = function(client, bufnr)
--   local function buf_set_option(...)
--     vim.api.nvim_buf_set_option(bufnr, ...)
--   end
--   --Enable completion triggered by <c-x><c-o>
--   buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")
-- end
-- you can overwrite the null_ls setup table (useful for setting the root_dir function)
-- lvim.lsp.null_ls.setup = {
--   root_dir = require("lspconfig").util.root_pattern("Makefile", ".git", "node_modules"),
-- }
-- or if you need something more advanced
-- lvim.lsp.null_ls.setup.root_dir = function(fname)
--   if vim.bo.filetype == "javascript" then
--     return require("lspconfig/util").root_pattern("Makefile", ".git", "node_modules")(fname)
--       or require("lspconfig/util").path.dirname(fname)
--   elseif vim.bo.filetype == "php" then
--     return require("lspconfig/util").root_pattern("Makefile", ".git", "composer.json")(fname) or vim.fn.getcwd()
--   else
--     return require("lspconfig/util").root_pattern("Makefile", ".git")(fname) or require("lspconfig/util").path.dirname(fname)
--   end
-- end

-- set a formatter if you want to override the default lsp one (if it exists)
-- lvim.lang.python.formatters = {
--   {
--     exe = "black",
--   }
-- }
-- set an additional linter
-- lvim.lang.python.linters = {
--   {
--     exe = "flake8",
--   }
-- }

lvim.builtin.which_key.mappings["gh"] = {
  "<cmd>Telescope git_bcommits<cr>", "Buffer commit history"
}
lvim.builtin.which_key.mappings["gH"] = {
  "<cmd>Telescope git_commits<cr>", "Commit history"
}

-- Additional Plugins
lvim.plugins = {
  {"tpope/vim-surround"},
  {"tpope/vim-characterize"},
  {"tpope/vim-speeddating"},
  {"tpope/vim-repeat"},
  {"vimwiki/vimwiki"},
  {"gpanders/vim-medieval"},
  {"tools-life/taskwiki"},
}

vim.g.medieval_langs = {"bash", "sh", "python"}

-- Autocommands (https://neovim.io/doc/user/autocmd.html)
-- lvim.autocommands.custom_groups = {
--   { "BufWinEnter", "*.lua", "setlocal ts=8 sw=8" },
-- }
