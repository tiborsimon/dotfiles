"==============================================================================
" KEYBOARD MAPPINGS
"==============================================================================
let mapleader="\<Space>"

" Remap the semicolon to be able to use the colon faster.
nnoremap ; :
vnoremap ; :

" Navigate through wrapped long lines intuitively.
nnoremap j gj
nnoremap k gk

" Making a more ergonomic esc. Ctr+[ is better!
"inoremap jk <esc>
"inoremap kj <esc>

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
nnoremap J :move .+1<cr>==
nnoremap K :move .-2<cr>==
vnoremap J :move '>+1<cr>gv=gv
vnoremap K :move '<-2<cr>gv=gv

" Indented new line, useful in HTML files
inoremap <C-CR> <CR><Esc>k$o

lua <<EOF
local wk = require("which-key")
wk.register({
  ["<leader>f"] = {
    name = "+[F]ile operations",
    s = { "<cmd>w<cr>", "[S]ave File", silent=False },
    S = { "<cmd>wa<cr>", "[S]ave All Files", silent=False },
  },
})
wk.register({
  ["<leader>t"] = {
    name = "+[T]oggle",
    w = { "<cmd>setlocal wrap!<cr>", "Toggle [W]rap" },
  },
})
EOF
