" Vimrc of Tibor Simon
" Started on 2016-03-14

"General Vim config
  colorscheme torte
  syntax on

  " Solving paste issue with comments
  set pastetoggle=
  set nosi noai

  set clipboard=unnamedplus

  " runtime macros/matchit.vim

  " Tab management: http://stackoverflow.com/a/1878983/1565331
  set tabstop=2
  set softtabstop=2
  set expandtab
  set shiftwidth=2
  set smarttab
  set smartindent
  set autoindent


  set foldmethod=marker
  set foldmarker={{{,}}}
  set number
  set ruler
  set hidden

  " Encoding
  set encoding=utf-8
  set termencoding=utf-8

  set backspace=indent,eol,start

  " Diff options
  set diffopt+=vertical
  set splitbelow
  set splitright

  " let g:loaded_matchparen=1

  " Backup and Swap
  set nobackup
  set nowritebackup
  set noswapfile

  " Genral UI settings
  set laststatus=2       " Always show the statusline
  set noshowmode         " Hide the default mode text (e.g. -- INSERT -- below the statusline)
  set showmatch          " Shows matching brackets when text indicator is over them
  set scrolloff=5        " Show 5 lines of context around the cursor
  set sidescrolloff=20
  set lazyredraw         " The screen won't be redrawn unless actions took place
  set scrolljump=0
  set showcmd
  set ttyfast            " Improves redrawing for newer computers
  set pumheight=10       " Max number of popup menu items
  set diffopt+=context:3
  set nostartofline      " When moving thru the lines, the cursor will try to stay in the previous columns"

  " Search Options
  set hlsearch           " Highlight search
  set incsearch          " Incremental search
  set magic              " Set magic on, for regular expressions
  set ignorecase         " Searches are Non Case-sensitive
  set smartcase          " Overrides ignorecase, if search contains uppercase character


"GUI settings
  set fillchars+=vert:\ "space
  hi StatusLine     ctermfg=233 ctermbg=246
  hi StatusLineNC   ctermfg=239 ctermbg=245
  hi VertSplit      ctermfg=237 ctermbg=240

  hi Folded         ctermfg=241 ctermbg=233
  hi MatchParen cterm=none ctermbg=240 ctermfg=7

  hi Search cterm=NONE ctermfg=0 ctermbg=11
  hi Visual cterm=NONE ctermfg=blue ctermbg=white

  " Redefine diff colors:
  " http://vim.wikia.com/wiki/Xterm256_color_names_for_console_Vim
  highlight DiffAdd    ctermfg=10 ctermbg=22
  highlight DiffChange ctermfg=7 ctermbg=8
  highlight DiffText   ctermfg=15 ctermbg=27
  highlight DiffDelete ctermfg=9 ctermbg=88


"Basic keyboard remappings
  " Essentials
  let mapleader="\<Space>"
  nnoremap ; :
  vnoremap ; :
  nnoremap j gj
  nnoremap k gk
  set pastetoggle=<leader><F3>

  " Vimrc handling
  nnoremap <silent> <leader>vrc :tabe ~/.vimrc<CR>
  nnoremap <leader>l :so ~/.vimrc<cr>

  "General make mapping
  nnoremap <leader>mm :!clear && make<cr>
  nnoremap <leader>mt :!clear && make test<cr>
  nnoremap <leader>mc :!clear && make clean<cr>
  nnoremap <leader>mb :!clear && make build<cr>
  nnoremap <leader>m<leader> :!clear && make 
  nnoremap <leader>p8 :BookmarkAnnotate pep8<cr>

  " Language specific mappings
  autocmd Filetype python nnoremap <buffer> <leader>u :! clear && python -m unittest discover<cr>
  autocmd Filetype python nnoremap <buffer> <leader>vu :! clear && python -m unittest discover -v<cr>
  autocmd Filetype python nnoremap <buffer> <leader>fu :! clear && python -m unittest discover -f<cr>
  autocmd Filetype python nnoremap <buffer> <leader>ndb Ofrom nose.tools import set_trace; set_trace()<esc>j^:w<cr>
  autocmd Filetype python nnoremap <buffer> <leader>pdb Oimport pdb; pdb.set_trace()<esc>j^:w<cr>
  autocmd Filetype python nnoremap <buffer> <leader>pudb Oimport pudb; pudb.set_trace()<esc>j^:w<cr>
  autocmd Filetype python nnoremap <buffer> <leader>ipdb Oimport ipdb; ipdb.set_trace()<esc>j^:w<cr>
  autocmd Filetype python nnoremap <buffer> <leader>p3 :! clear && python3 %<cr>
  autocmd Filetype python nnoremap <buffer> <leader>p2 :! clear && python2 %<cr>
  autocmd BufRead *.py set smartindent cinwords=if,elif,elelsse,for,while,try,except,finally,def,class

  autocmd BufReadPre * let g:gitgutter_enabled = 1

  " Convenient mappings
  nnoremap <leader>w :w<cr>
  nnoremap <leader>q :q<cr>

  " Toggle wrap mode
  nnoremap <leader>tw :set wrap!<cr>

  " Clear search
  nnoremap <silent><leader>/ /asldkfjasogihasjhfoashdkcjashdfhsf<cr>

  " Toggle scroll bind
  nnoremap <leader>ts :set scb!<cr>

  " Diff handling
  nnoremap <leader>dt :diffthis<cr>
  nnoremap <leader>do :diffoff!<cr>

  " Tab size changind - default is 2
  nnoremap <leader><f4> :set ts=4 sw=4<cr>
  nnoremap <leader><F2> :set ts=2 sw=2<cr>

  " Window related remappings
  nnoremap <leader>sv <C-w>v
  nnoremap <leader>sh <C-w>s
  nnoremap <F9> :vertical resize +1<cr>
  nnoremap <F6> :vertical resize -1<cr>
  nnoremap <F7> :resize +1<cr>
  nnoremap <F8> :resize -1<cr>
  nnoremap <leader>vsa :vert sba<cr>
  nnoremap <C-h> <C-w>h
  nnoremap <C-j> <C-w>j
  nnoremap <C-k> <C-w>k
  nnoremap <C-l> <C-w>l

  " Insert date
  nnoremap <leader>id :0read !insert-date<cr>o

  " Tab related remappings
  map <leader>Tn :tabnew<CR>
  map <leader>Tc :tabclose<CR>

  " Motion in insert mode
  inoremap <C-h> <left>
  inoremap <C-l> <right>
  inoremap <C-j> <down>
  inoremap <C-k> <up>

  " Move lines in all modes
  nnoremap J :m .+1<CR>==
  nnoremap K :m .-2<CR>==
  vnoremap J :m '>+1<CR>gv=gv
  vnoremap K :m '<-2<CR>gv=gv
