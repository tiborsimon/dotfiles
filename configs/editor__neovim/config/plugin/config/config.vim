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

