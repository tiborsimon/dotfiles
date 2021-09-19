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
" BASIC SETTINGS
"=============================================================================
set nocompatible

syntax enable

set number
set relativenumber

set ignorecase
set smartcase

" Disable backup and swap files as we are using version control tools
set nobackup
set nowritebackup
set noswapfile

" Splits open at the bottom and right, which is non-retarded
set splitbelow splitright

set undofile
set undodir=~/.cache/nvim/undodir

" Tab management: http://stackoverflow.com/a/1878983/1565331
set tabstop=2
set softtabstop=2
set expandtab
set shiftwidth=2
set smarttab
set smartindent
set autoindent

" Scrolling
set scrolloff=3
set sidescrolloff=5
set scrolljump=0

"=============================================================================
" KEYBOARD MAPPINGS
"=============================================================================
let mapleader="\<Space>"

" Remap the semicolon to be able to use the colon faster.
nnoremap ; :
vnoremap ; :

" Navigate through wrapped long lines intuitively.
nnoremap j gj
nnoremap k gk

" Save and quit fast access
nnoremap <leader>w :write<cr>
nnoremap <leader>q :quit<cr>

" Toggle wrap mode
nnoremap <leader>tw :setlocal wrap!<cr>

" Easyer split navigation
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" Motion in insert mode
inoremap <C-h> <left>
inoremap <C-l> <right>
inoremap <C-j> <down>
inoremap <C-k> <up>

" Motion in command line
cnoremap <C-h> <left>
cnoremap <C-l> <right>
cnoremap <C-j> <down>
cnoremap <C-k> <up>

" Move lines in all modes
nnoremap J :m .+1<cr>==
nnoremap K :m .-2<cr>==
vnoremap J :m '>+1<cr>gv=gv
vnoremap K :m '<-2<cr>gv=gv

" Indented new line, useful in HTML files
inoremap <C-CR> <CR><Esc>k$o


"=============================================================================
" CLIPBOARD HANDLING
"=============================================================================

vnoremap <C-c> "*y :let @+=@*<cr>
nnoremap <C-P> "+p


"=============================================================================
" SEARCH CONFIG
"=============================================================================

" Search options
set hlsearch           " Highlight search
set incsearch          " Incremental search
set magic              " Set magic on, for regular expressions
set ignorecase         " Searches are Non Case-sensitive
set smartcase          " Overrides ignorecase, if search contains uppercase character


" Do not jump if a word is selected
nnoremap * :let @/='\<<C-R>=expand("<cword>")<cr>\>'<cr>:set hls<cr>
nnoremap <cr> :let @/='\<<C-R>=expand("<cword>")<cr>\>'<cr>:set hls<cr>

" Enable visual selection search patterns
" Source: http://vim.wikia.com/wiki/Highlight_all_search_pattern_matches
set guioptions+=a
function! MakePattern(text)
  let pat = escape(a:text, '\')
  let pat = substitute(pat, '\_s\+$', '\\s\\*', '')
  let pat = substitute(pat, '^\_s\+', '\\s\\*', '')
  let pat = substitute(pat, '\_s\+',  '\\_s\\+', 'g')
  return '\\V' . escape(pat, '\"')
endfunction
vnoremap <silent> <cr> :<C-U>let @/="<C-R>=MakePattern(@*)<cr>"<cr>:set hls<cr>

" Disables the search highlight for the next search
nnoremap <silent> <leader>/ :nohlsearch<cr>


"=============================================================================
" PLUGINS
"=============================================================================
call plug#begin(stdpath('data') . 'vimplug')
  Plug 'joeytwiddle/sexy_scroller.vim'

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

  Plug 'hoob3rt/lualine.nvim'
  Plug 'kyazdani42/nvim-web-devicons'
  Plug 'ryanoasis/vim-devicons'
  Plug 'kdheepak/tabline.nvim'
  Plug 'arkav/lualine-lsp-progress'

  Plug 'bronson/vim-trailing-whitespace'

  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-surround'
  Plug 'tpope/vim-characterize'
  Plug 'tpope/vim-speeddating'
  Plug 'tpope/vim-repeat'

  Plug 'junegunn/vim-easy-align'

  Plug 'norcalli/nvim-colorizer.lua'

  Plug 'preservim/nerdcommenter'

  Plug 'mattn/emmet-vim', { 'for': ['html'] }

  Plug 'mg979/vim-visual-multi', {'branch': 'master'}

  Plug 'kevinhwang91/nvim-hlslens'

  Plug 'Yggdroot/indentLine'

  Plug 'Raimondi/delimitMate'

call plug#end()


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
" PLUGIN - GALAXYLINE
"=============================================================================

lua <<EOF
require("lsp_config")
require("completion_config")
require("treesitter_config")
EOF

"=============================================================================
" PLUGIN - VIM-SMOOTHIE
"=============================================================================

" let g:smoothie_experimental_mappings=1
let g:SexyScroller_MaxTime = 400
let g:SexyScroller_EasingStyle = 3

"=============================================================================
" PLUGIN - EASY-MOTION
"=============================================================================

let g:EasyMotion_do_mapping = 0
let g:EasyMotion_smartcase = 1

" <Leader>f{char} to move to {char}
map  <Leader>j <Plug>(easymotion-bd-f2)
nmap <Leader>j <Plug>(easymotion-overwin-f2)

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

nnoremap <leader>ff <cmd>Telescope find_files<cr>
nnoremap <leader>fF <cmd>Telescope file_browser<cr>
nnoremap <leader>fo <cmd>Telescope oldfiles<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>

" VIM pickers
nnoremap <leader>b <cmd>Telescope buffers<cr>
nnoremap <leader>vh <cmd>Telescope help_tags<cr>
nnoremap <leader>vc <cmd>Telescope colorscheme<cr>
nnoremap <leader>vs <cmd>Telescope search_history<cr>
nnoremap <leader>vr <cmd>Telescope registers<cr>
nnoremap <leader>vf <cmd>Telescope current_buffer_fuzzy_find<cr>
nnoremap <leader>vk <cmd>Telescope keymaps<cr>

" TREESITTER pickers
nnoremap <leader>tt <cmd>Telescope treesitter<cr>

" LSP pickers
nnoremap <leader>ls <cmd>Telescope lsp_document_symbols<cr>
nnoremap <leader>lS <cmd>Telescope lsp_workspace_symbols<cr>
nnoremap <leader>la <cmd>Telescope lsp_code_actions<cr>
nnoremap <leader>lr <cmd>Telescope lsp_references<cr>
nnoremap <leader>ld <cmd>Telescope lsp_definitions<cr>
nnoremap <leader>li <cmd>Telescope lsp_implementations<cr>

" GIT pickers
nnoremap <leader>gc <cmd>Telescope git_bcommits<cr>
nnoremap <leader>gC <cmd>Telescope git_commits<cr>
nnoremap <leader>gb <cmd>Telescope git_branches<cr>
nnoremap <leader>gs <cmd>Telescope git_status<cr>
nnoremap <leader>gS <cmd>Telescope git_stash<cr>

lua <<EOF
require("telescope_config")
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

nnoremap <silent> <F2> :NvimTreeToggle<CR>
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

noremap <silent> n <Cmd>execute('normal! ' . v:count1 . 'n')<CR>
            \<Cmd>lua require('hlslens').start()<CR>

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

let g:indentLine_char = '▏'
