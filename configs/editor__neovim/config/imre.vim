"=============================================================================
"   _       _ _         _
"  (_)     (_) |       (_)
"   _ _ __  _| |___   ___ _ __ ___
"  | | '_ \| | __\ \ / / | '_ ` _ \
"  | | | | | | |_ \ V /| | | | | | |
"  |_|_| |_|_|\__(_)_/ |_|_| |_| |_|
"
"=============================================================================
" NEOVIM CONFIG
"=============================================================================
"  Started on 2021-08-01
"
"=============================================================================
" PLUGINS
"=============================================================================
call plug#begin(stdpath('data') . 'vimplug')
  Plug 'psliwka/vim-smoothie'

  Plug 'monsonjeremy/onedark.nvim'

  Plug 'easymotion/vim-easymotion'

  Plug 'neovim/nvim-lspconfig'
  Plug 'kabouzeid/nvim-lspinstall'
  Plug 'glepnir/lspsaga.nvim'

  Plug 'nvim-lua/popup.nvim'
  Plug 'nvim-lua/plenary.nvim'
  Plug 'nvim-telescope/telescope.nvim'
  Plug 'nvim-telescope/telescope-fzy-native.nvim'

  Plug 'hrsh7th/nvim-compe'
  Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate', 'branch': '0.5-compat'}
  Plug 'nvim-treesitter/nvim-treesitter-textobjects'

  Plug 'lewis6991/gitsigns.nvim'

  Plug 'kyazdani42/nvim-web-devicons' " for file icons
  Plug 'kyazdani42/nvim-tree.lua'

  Plug 'kana/vim-arpeggio'
  Plug 'folke/zen-mode.nvim'

  Plug 'folke/which-key.nvim'

  Plug 'hoob3rt/lualine.nvim'
  Plug 'kyazdani42/nvim-web-devicons'
  Plug 'ryanoasis/vim-devicons'
  Plug 'kdheepak/tabline.nvim'
  Plug 'arkav/lualine-lsp-progress'

  Plug 'akinsho/toggleterm.nvim'

  Plug 'bronson/vim-trailing-whitespace'

  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-surround'
  Plug 'tpope/vim-characterize'
  Plug 'tpope/vim-speeddating'
  Plug 'tpope/vim-repeat'

  Plug 'junegunn/vim-easy-align'

  Plug 'norcalli/nvim-colorizer.lua'

  " Plug 'preservim/nerdcommenter'

  Plug 'mattn/emmet-vim', { 'for': ['html'] }

  Plug 'mg979/vim-visual-multi', {'branch': 'master'}

  " Plug 'kevinhwang91/nvim-hlslens'

  " Plug 'Yggdroot/indentLine'

  " Plug 'Raimondi/delimitMate'

call plug#end()


lua <<EOF
require("toggleterm").setup{
  -- size can be a number or function which is passed the current terminal

  -- size = 20,
  -- open_mapping = [[<c-\>]],
  -- hide_numbers = true, -- hide the number column in toggleterm buffers
  -- shade_filetypes = {},
  -- shade_terminals = true,
  -- shading_factor = '<number>', -- the degree by which to darken to terminal colour, default: 1 for dark backgrounds, 3 for light
  -- start_in_insert = true,
  -- insert_mappings = true, -- whether or not the open mapping applies in insert mode
  -- persist_size = true,
  -- direction = 'float',
  -- close_on_exit = true, -- close the terminal window when the process exits
  -- shell = vim.o.shell, -- change the default shell
  -- -- This field is only relevant if direction is set to 'float'
  -- float_opts = {
  --   -- The border key is *almost* the same as 'nvim_open_win'
  --   -- see :h nvim_open_win for details on borders however
  --   -- the 'curved' border is a custom border type
  --   -- not natively supported but implemented in this plugin.
  --   border = 'curved',
  --   --     width = <value>,
  --   -- height = <value>,
  --   winblend = 3,
  --   highlights = {
  --     border = "Normal",
  --     background = "Normal",
  --   }
  -- }
}
EOF

lua <<EOF
local Terminal  = require('toggleterm.terminal').Terminal
local lazygit = Terminal:new({ hidden = true, position = float })

function _lazygit_toggle()
  lazygit:toggle()
end

vim.api.nvim_set_keymap("n", "<c-\\>", "<cmd>lua _lazygit_toggle()<CR>", {noremap = true, silent = true})
EOF


"=============================================================================
" GUI THEME
"=============================================================================
set termguicolors
set cursorline

lua <<EOF
require("onedark").setup({
  commentStyle = "bold,italic",
  keywordStyle = "bold",
  variableStyle = "bold",
  functionStyle = "bold",
  sidebars = {},
  darkSidebar = true,
  hideInactiveStatusline = true,
  colors = {
    hint = "orange",
    fg_gutter = "#40444a",
  }
})
EOF
colorscheme onedark

"=============================================================================
" PLUGIN - WHICH-KEY
"=============================================================================

lua << EOF
  require("which-key").setup {
    plugins = {
      marks = false,
      registers = false,
      spelling = {
        enabled = false,
        suggestions = 20,
      },
    }
  }
EOF

"=============================================================================
" PLUGIN - GALAXYLINE
"=============================================================================

lua <<EOF
require("lsp_config")
require("completion_config")
require("treesitter_config")
EOF

"=============================================================================
" PLUGIN - SEXY-SCROLLER
"=============================================================================
" Set the time it takes (in milliseconds) for the buffer to scroll one line
" or column.
let g:SexyScroller_ScrollTime = 11

" Set the time it takes for the cursor to travel one line. Probably only
" visible if you have `:set cursorline`.  Set it to 0 to never animate
" the cursor.
let g:SexyScroller_CursorTime = 5

" Set the maximum amount of time that longer scrolls can take.
let g:SexyScroller_MaxTime = 300

" Choose the easing style (how scrolling accelerates and deccelerates):
"   - 1 = start fast, finish slowly            (like 2 but less so)
"   - 2 = start very fast, finish very slowly  (recommended, default)
"   - 3 = start slowly, get faster, end slowly (sexy)
"   - 4 = start very slowly, end very slowly   (like 3 but more so)
"   - ? = constant speed                       (dull)
let g:SexyScroller_EasingStyle = 3

" Interrupts the animation if you press a key.  Resumes the animation if they
" key you pressed causes further scrolling, otherwise just jumps directly to
" the destination.  Resuming animation looks best with EasingStyle 1 or 2.
let g:SexyScroller_DetectPendingKeys = 1

"=============================================================================
" PLUGIN - EASY-MOTION
"=============================================================================

let g:EasyMotion_do_mapping = 0
let g:EasyMotion_smartcase = 1

" <Leader>f{char} to move to {char}
map  <Leader><space> <Plug>(easymotion-bd-f2)
nmap <Leader><space> <Plug>(easymotion-overwin-f2)

"=============================================================================
" PLUGIN - LSP
"=============================================================================
"
" >> Lsp key bindings
" nnoremap <silent> gd    <cmd>lua vim.lsp.buf.definition()<CR>
" nnoremap <silent> <C-]> <cmd>lua vim.lsp.buf.definition()<CR>
" nnoremap <silent> gD    <cmd>lua vim.lsp.buf.declaration()<CR>
" nnoremap <silent> gr    <cmd>lua vim.lsp.buf.references()<CR>
" nnoremap <silent> gi    <cmd>lua vim.lsp.buf.implementation()<CR>
" " nnoremap <silent> K     <cmd>Lspsaga hover_doc<CR>
" nnoremap <silent> <C-k> <cmd>lua vim.lsp.buf.signature_help()<CR>
" nnoremap <silent> <C-p> <cmd>Lspsaga diagnostic_jump_prev<CR>
" nnoremap <silent> <C-n> <cmd>Lspsaga diagnostic_jump_next<CR>
" nnoremap <silent> gf    <cmd>lua vim.lsp.buf.formatting()<CR>
" nnoremap <silent> gn    <cmd>lua vim.lsp.buf.rename()<CR>
" nnoremap <silent> ga    <cmd>Lspsaga code_action<CR>
" xnoremap <silent> ga    <cmd>Lspsaga range_code_action<CR>
" nnoremap <silent> gs    <cmd>Lspsaga signature_help<CR>

"=============================================================================
" PLUGIN - TELESCOPE
"=============================================================================


" VIM pickers

" TREESITTER pickers
nnoremap <leader>tt <cmd>Telescope treesitter<cr>

" LSP pickers
nnoremap <leader>ls <cmd>Telescope lsp_document_symbols<cr>
nnoremap <leader>lS <cmd>Telescope lsp_workspace_symbols<cr>
nnoremap <leader>la <cmd>Telescope lsp_code_actions<cr>
nnoremap <leader>lr <cmd>Telescope lsp_references<cr>
nnoremap <leader>ld <cmd>Telescope lsp_definitions<cr>
nnoremap <leader>li <cmd>Telescope lsp_implementations<cr>




lua <<EOF
require("telescope_config")
EOF

lua <<EOF
local wk = require("which-key")
wk.register({
  f = {
    name = "Files",
    f = { "<cmd>Telescope find_files<cr>", "[F]ind File" },
    b = { "<cmd>Telescope file_browser<cr>", "File [B]rowser" },
    r = { "<cmd>Telescope oldfiles<cr>", "[R]ecent Files" },
    g = { "<cmd>Telescope live_grep<cr>", "Live [G]rep" },
  },
}, { prefix = "<leader>" })
EOF

lua <<EOF
local wk = require("which-key")
wk.register({
  v = {
    name = "Vim",
    h = { "<cmd>Telescope help_tags<cr>", "[H]elp Tags" },
    c = { "<cmd>Telescope colorscheme<cr>", "[C]olorscheme" },
    s = { "<cmd>Telescope search_history<cr>", "[S]earch History" },
    r = { "<cmd>Telescope registers<cr>", "[R]egisters" },
    f = { "<cmd>Telescope current_buffer_fuzzy_find<cr>", "Buffer [F]uzzy Find" },
    k = { "<cmd>Telescope keymaps<cr>", "[K]eymaps" },
  },
}, { prefix = "<leader>" })
EOF

lua <<EOF
local wk = require("which-key")
wk.register({
  b = {
    name = "Buffers",
    b = { "<cmd>Telescope buffers<cr>", "List [B]uffers" },
  },
}, { prefix = "<leader>" })
EOF

lua <<EOF
local wk = require("which-key")
wk.register({
  b = {
    name = "Buffers",
    b = { "<cmd>Telescope buffers<cr>", "List [B]uffers" },
  },
}, { prefix = "<leader>" })
EOF

lua <<EOF
local wk = require("which-key")
wk.register({
  g = {
    name = "Git",
    b = { "<cmd>Telescope git_bcommits<cr>", "[B]uffer Commits" },
    c = { "<cmd>Telescope git_commits<cr>", "[C]ommits" },
    s = { "<cmd>Telescope git_status<cr>", "[S]tatus" },
    S = { "<cmd>Telescope git_stash<cr>", "[S]tash" },
    B = { "<cmd>Telescope git_branches<cr>", "[B]ranches" },
  },
}, { prefix = "<leader>" })
EOF

"=============================================================================
" PLUGIN - GITSIGNS
"=============================================================================

lua <<EOF
require("gitsigns").setup()
EOF

nnoremap <leader>gb <cmd>Gitsigns blame_line<cr>

nnoremap <leader>ghh <cmd>Gitsigns preview_hunk<cr>
nnoremap <leader>ghn <cmd>Gitsigns next_hunk<cr>
nnoremap <leader>ghp <cmd>Gitsigns prev_hunk<cr>

nnoremap <leader>gtl <cmd>Gitsigns toggle_linehl<cr>
nnoremap <leader>gtn <cmd>Gitsigns toggle_numhl<cr>
nnoremap <leader>gts <cmd>Gitsigns toggle_signs<cr>
nnoremap <leader>gtb <cmd>Gitsigns toggle_current_line_blame<cr>

"=============================================================================
" PLUGIN - ARPEGGIO
"=============================================================================

augroup vim_enter_group
  autocmd!
  autocmd VimEnter * :call ExecuteAfterVimEntered()
augroup END

function ExecuteAfterVimEntered()
  if exists(":Arpeggio")
    " Map the jk key chord to send an <Esc> key press
    call arpeggio#map('iv', '', 0, 'jk', '<Esc>')
  endif
endfunction

"=============================================================================
" PLUGIN - ZEN-MODE
"=============================================================================

nnoremap <silent> <leader>z :ZenMode<cr>

lua << EOF
  require("zen-mode").setup {
    window = {
      backdrop = 1, -- shade the backdrop of the Zen window. Set to 1 to keep the same as Normal
      -- height and width can be:
      -- * an absolute number of cells when > 1
      -- * a percentage of the width / height of the editor when <= 1
      -- * a function that returns the width or the height
      width = 80, -- width of the Zen window
      height = 0.82, -- height of the Zen window
      -- by default, no options are changed for the Zen window
      -- uncomment any of the options below, or add other vim.wo options you want to apply
      options = {
        -- signcolumn = "no", -- disable signcolumn
        number = false, -- disable number column
        relativenumber = false, -- disable relative numbers
        -- cursorline = false, -- disable cursorline
        -- cursorcolumn = false, -- disable cursor column
        -- foldcolumn = "0", -- disable fold column
        -- list = false, -- disable whitespace characters
      },
    },
    plugins = {
      -- disable some global vim options (vim.o...)
      -- comment the lines to not apply the options
      options = {
        enabled = true,
        ruler = false, -- disables the ruler text in the cmd line area
        showcmd = false, -- disables the command in the last line of the screen
      },
      -- twilight = { enabled = true }, -- enable to start Twilight when zen mode opens
      gitsigns = { enabled = false }, -- disables git signs
      -- kitty = {
      --   enabled = true,
      --   font = "+10", -- font size increment
      -- },
    },
    on_open = function(win)
      vim.cmd("set nocursorline")
    end,
    on_close = function()
      vim.cmd("set cursorline")
    end,
  }
EOF

"=============================================================================
" PLUGIN - LUALINE
"=============================================================================

nnoremap <silent> gb :TablineBufferNext<cr>

lua << EOF

local colors = {
  yellow = '#ECBE7B',
  cyan = '#008080',
  darkblue = '#081633',
  green = '#98be65',
  orange = '#FF8800',
  violet = '#a9a1e1',
  magenta = '#c678dd',
  blue = '#51afef',
  red = '#ec5f67'
}

require'tabline'.setup { enable = false }
require('lualine').setup {
  options = {
    icons_enabled = true,
    theme = 'onedark',
    component_separators = {'', ''},
    section_separators = {'', ''},
    disabled_filetypes = {}
  },
  sections = {
    lualine_a = {'mode'},
    lualine_b = {'filename'},
    lualine_c = {'branch', {
	'lsp_progress',
	display_components = { 'lsp_client_name', { 'title', 'percentage', 'message' }},
	-- With spinner
	-- display_components = { 'lsp_client_name', 'spinner', { 'title', 'percentage', 'message' }},
	colors = {
	  percentage  = colors.cyan,
	  title  = colors.cyan,
	  message  = colors.cyan,
	  spinner = colors.cyan,
	  lsp_client_name = colors.magenta,
	  use = true,
	},
	separators = {
		component = ' ',
		progress = ' | ',
		message = { pre = '(', post = ')'},
		percentage = { pre = '', post = '%% ' },
		title = { pre = '', post = ': ' },
		lsp_client_name = { pre = '[', post = ']' },
		spinner = { pre = '', post = '' },
		message = { commenced = 'In Progress', completed = 'Completed' },
	},
	display_components = { 'lsp_client_name', 'spinner', { 'title', 'percentage', 'message' } },
	timer = { progress_enddelay = 500, spinner = 1000, lsp_client_name_enddelay = 1000 },
	spinner_symbols = { '🌑 ', '🌒 ', '🌓 ', '🌔 ', '🌕 ', '🌖 ', '🌗 ', '🌘 ' },
}},
    lualine_x = {'encoding', 'fileformat', 'filetype'},
    lualine_y = {'progress'},
    lualine_z = {'location'}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {'filename'},
    lualine_x = {'location'},
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = { require'tabline'.tabline_buffers },
    lualine_x = { require'tabline'.tabline_tabs },
    lualine_y = {},
    lualine_z = {},
  },
  extensions = {'nvim-tree'}
}

EOF

"=============================================================================
" PLUGIN - VIM-TRAILING-WHITESPACE
"=============================================================================

let g:extra_whitespace_ignored_filetypes = ["TelescopePrompt", "vim-plug", "[Plugins]"]

"=============================================================================
" PLUGIN - NVIM TREE
"=============================================================================

lua <<EOF
  require'nvim-tree'.setup()
EOF

let g:nvim_tree_show_icons = {
    \ 'git': 0,
    \ 'folders': 0,
    \ 'files': 0,
    \ 'folder_arrows': 1,
    \ }
let g:nvim_tree_disable_default_keybindings = 1

lua <<EOF
  local tree_cb = require'nvim-tree.config'.nvim_tree_callback
  vim.g.nvim_tree_bindings = {
    { key = {"<CR>", "o"}, cb = tree_cb("edit") },
    { key = "e",           cb = tree_cb("cd") },
    { key = "s",           cb = tree_cb("vsplit") },
    { key = "i",           cb = tree_cb("split") },
    { key = "t",           cb = tree_cb("tabnew") },
    { key = "<",           cb = tree_cb("prev_sibling") },
    { key = ">",           cb = tree_cb("next_sibling") },
    { key = "P",           cb = tree_cb("parent_node") },
    { key = "<BS>",        cb = tree_cb("close_node") },
    { key = "<S-CR>",      cb = tree_cb("close_node") },
    { key = "<Tab>",       cb = tree_cb("preview") },
    { key = "K",           cb = tree_cb("first_sibling") },
    { key = "J",           cb = tree_cb("last_sibling") },
    { key = "I",           cb = tree_cb("toggle_ignored") },
    { key = "H",           cb = tree_cb("toggle_dotfiles") },
    { key = "R",           cb = tree_cb("refresh") },
    { key = "a",           cb = tree_cb("create") },
    { key = "d",           cb = tree_cb("remove") },
    { key = "r",           cb = tree_cb("rename") },
    { key = "<C-r>",       cb = tree_cb("full_rename") },
    { key = "x",           cb = tree_cb("cut") },
    { key = "c",           cb = tree_cb("copy") },
    { key = "p",           cb = tree_cb("paste") },
    { key = "y",           cb = tree_cb("copy_name") },
    { key = "Y",           cb = tree_cb("copy_path") },
    { key = "gy",          cb = tree_cb("copy_absolute_path") },
    { key = "[c",          cb = tree_cb("prev_git_item") },
    { key = "]c",          cb = tree_cb("next_git_item") },
    { key = "u",           cb = tree_cb("dir_up") },
    { key = "O",           cb = tree_cb("system_open") },
    { key = "q",           cb = tree_cb("close") },
    { key = "?",           cb = tree_cb("toggle_help") },
  }
EOF
nnoremap <silent> <F2> :NvimTreeToggle<CR>

"=============================================================================
" PLUGIN - COLORIZE
"=============================================================================

lua <<EOF
require 'colorizer'.setup()
EOF

"=============================================================================
" PLUGIN - NERDCOMMENTER
"=============================================================================

" Create default mappings
let g:NERDCreateDefaultMappings = 1
let g:NERDSpaceDelims = 1

"=============================================================================
" PLUGIN - EASY ALIGN
"=============================================================================

" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap <leader>a <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap <leader>a <Plug>(EasyAlign)

"=============================================================================
" PLUGIN - HLS LENS
"=============================================================================

"noremap <silent> n <Cmd>execute('normal! ' . v:count1 . 'n')<CR>
"            \<Cmd>lua require('hlslens').start()<CR>

" "=============================================================================
" " PLUGIN - EMMET VIM
" "=============================================================================

" let g:user_emmet_leader_key='<C-m>'
" let g:user_emmet_install_global = 0
" autocmd FileType html,css EmmetInstall
" let g:user_emmet_mode='i'

"=============================================================================
" PLUGIN - INDENT LINE
"=============================================================================

" let g:indentLine_char = '▏'
