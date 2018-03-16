" Vimrc of Tibor Simon
" Started on 2016-03-14

"General Vim config {{{
  colorscheme torte

  " Solving paste issue with comments
  set pastetoggle=
  set nosi noai

  set clipboard=unnamedplus

  runtime macros/matchit.vim

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

"}}} General Vim config

"Basic keyboard remappings {{{
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

"}}} Basic keyboard remappings

"GUI settings {{{
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
"}}} GUI settings

"Plugins {{{
  call plug#begin('~/.vim/plugged')
    " Plug '907th/vim-auto-save'
    " Plug 'Valloric/YouCompleteMe'
    " Plug 'chrisbra/vim-diff-enhanced'
    " Plug 'mhinz/vim-startify'
    " Plug 'nvie/vim-flake8'
    " Plug 'scrooloose/syntastic'
    " Plug 'terryma/vim-expand-region'
    Plug 'MarcWeber/vim-addon-mw-utils'
    Plug 'MattesGroeger/vim-bookmarks'
    Plug 'Yggdroot/indentLine'
    Plug 'airblade/vim-gitgutter'
    Plug 'chrisbra/NrrwRgn'
    Plug 'ctrlpvim/ctrlp.vim'
    Plug 'dbakker/vim-projectroot'
    Plug 'dkprice/vim-easygrep'
    Plug 'easymotion/vim-easymotion'
    Plug 'peterhoeg/vim-qml'
    " Plug 'edkolev/tmuxline.vim'
    Plug 'elzr/vim-json'
    Plug 'ervandew/supertab'
    Plug 'fidian/hexmode'
    Plug 'garbas/vim-snipmate'
    Plug 'godlygeek/tabular'
    Plug 'gregsexton/MatchTag'
    Plug 'gregsexton/gitv'
    Plug 'honza/vim-snippets'
    Plug 'jamessan/vim-gnupg'
    Plug 'jceb/vim-orgmode'
    Plug 'jeetsukumaran/vim-buffergator'
    Plug 'jelera/vim-javascript-syntax'
    Plug 'jiangmiao/auto-pairs'
    Plug 'joeytwiddle/sexy_scroller.vim'
    Plug 'kana/vim-arpeggio'
    Plug 'kshenoy/vim-signature'
    Plug 'mattn/emmet-vim'
    Plug 'mbbill/undotree'
    Plug 'mileszs/ack.vim'
    Plug 'nathanalderson/yang.vim'
    Plug 'ntpeters/vim-better-whitespace'
    Plug 'pangloss/vim-javascript'
    Plug 'plasticboy/vim-markdown'
    Plug 'scrooloose/nerdcommenter'
    Plug 'scrooloose/nerdtree'
    Plug 'sukima/xmledit'
    Plug 'terryma/vim-multiple-cursors'
    Plug 'tpope/vim-commentary'
    Plug 'tpope/vim-dispatch'
    Plug 'tpope/vim-fugitive'
    Plug 'tpope/vim-repeat'
    Plug 'tpope/vim-rsi'
    Plug 'tpope/vim-speeddating'
    Plug 'tpope/vim-surround'
    Plug 'tpope/vim-unimpaired'
    Plug 'vim-airline/vim-airline'
    Plug 'vim-airline/vim-airline-themes'
    Plug 'vim-scripts/tlib'
    Plug 'vim-scripts/utl.vim'
  call plug#end()

  "Plugin: Ack {{{
    let g:AutoPairsMapCh = ''
  "}}} Plugin: Ack

  "Plugin: Ack {{{
    let g:ackhighlight = 1
    let g:ackpreview = 0
    let g:ack_autofold_results = 0
    " let g:ack_use_dispatch = 1
    noremap <Leader>a :Ack <cword><cr>
    noremap <C-a> :Ack
  "}}} Plugin: Ack

  "Plugin: AutoSave {{{
    " let g:auto_save = 1
  "}}} Plugin: AutoSave

  "Plugin: Airline {{{
    let g:airline_powerline_fonts = 1

    if !exists('g:airline_symbols')
      let g:airline_symbols = {}
    endif

    let g:airline_theme='powerlineish'
    let g:airline_section_z='%3p%% %{g:airline_symbols.linenr}%#__accent_bold#%4l%#__restore__#/%L:%3v'
    let g:airline_section_warning=''
    let g:airline#extensions#tabline#enabled = 1

    " unicode symbols
    let g:airline_left_sep = '»'
    let g:airline_left_sep = '▶'
    let g:airline_right_sep = '«'
    let g:airline_right_sep = '◀'
    let g:airline_symbols.linenr = '␊'
    let g:airline_symbols.linenr = '␤'
    let g:airline_symbols.linenr = '¶'
    let g:airline_symbols.branch = '⎇'
    let g:airline_symbols.paste = 'ρ'
    let g:airline_symbols.paste = 'Þ'
    let g:airline_symbols.paste = '∥'
    let g:airline_symbols.whitespace = 'Ξ'

    " airline symbols
    let g:airline_left_sep = ''
    let g:airline_left_alt_sep = ''
    let g:airline_right_sep = ''
    let g:airline_right_alt_sep = ''
    let g:airline_symbols.branch = ''
    let g:airline_symbols.readonly = ''
    let g:airline_symbols.linenr = ''"
    set laststatus=2
  "}}} Plugin: Airline

  "Plugin: Arpeggio {{{
    call arpeggio#map('iv', '', 0, 'jk', '<Esc>')
  "}}} Plugin: Arpeggio

  "Plugin: BetterWhitespace {{{
    nnoremap <leader>sw :StripWhitespace<cr>
  "}}} Plugin: BetterWhitespace

  "Plugin: Bookmarks {{{
    let g:bookmark_sign = '>>'
    let g:bookmark_annotation_sign = '##'
    let g:bookmark_auto_close = 1
    let g:bookmark_highlight_lines = 1
    let g:bookmark_save_per_working_dir = 1
    let g:bookmark_auto_save = 1
    let g:bookmark_center = 1
  "}}} Plugin: Bookmarks

  "Plugin: CtrlP {{{
    let g:ctrlp_map = '<c-p>'
    let g:ctrlp_cmd = 'CtrlPLastMode'
    let g:ctrlp_extensions = ['buffertag', 'tag', 'line', 'dir']
  "}}} Plugin: CtrlP

  "Plugin: Fugitive {{{
    nnoremap <silent> <leader>gs :Gstatus<CR>
    nnoremap <silent> <leader>gd :Gdiff<CR>
    nnoremap <silent> <leader>gc :Gcommit<CR>
    nnoremap <silent> <leader>gb :Gblame<CR>
    nnoremap <silent> <leader>gl :Glog<CR>
    nnoremap <silent> <leader>gp :Git push<CR>
    nnoremap <silent> <leader>gw :Gwrite<CR>
    nnoremap <silent> <leader>gr :Gremove<CR>
  "}}} Plugin: Fugitive

  "Plugin: Emmet {{{
    let g:emmet_html5 = 1
    let g:user_emmet_settings = {
    \    'indentation' : '  '
    \}
  "}}} Plugin: Emmet

  "Plugin: Enchanced diff {{{
    " started In Diff-Mode set diffexpr (plugin not loaded yet)
    if &diff
        let &diffexpr='EnhancedDiff#Diff("git diff", "--diff-algorithm=patience")'
    endif
  "}}} Plugin: Enchanced diff

  "Plugin: Gitv {{{
    nmap <leader>gv :Gitv --all<cr>
    nmap <leader>gV :Gitv! --all<cr>
    vmap <leader>gV :Gitv! --all<cr>
    let g:Gitv_DoNotMapCtrlKey = 1
    let g:Gitv_OpenHorizontal = 1
    let g:Gitv_WrapLines = 0
    let g:Gitv_OpenPreviewOnLaunch = 1
  "}}} Plugin: Gitv

  "Plugin: GitGutter {{{
    let g:gitgutter_max_signs = 2000
  "}}} Plugin: GitGutter

  "Plugin: Markdown {{{
    let g:vim_markdown_folding_disabled = 1
  "}}} Plugin: Markdown

  "Plugin: IndentLine {{{
    let g:indentLine_enabled = 0
    nnoremap <Leader>il :IndentLinesToggle<cr>
    let g:indentLine_color_term = 236
    let g:indentLine_char = '|'
  "}}} Plugin: IndentLine

  "Plugin: NERDTree {{{
    nnoremap <silent> <F2> :NERDTreeToggle<CR>
    let g:NERDTreeMapMenu = '<F3>'
    let g:NERDTreeChristmasTree = 1
    let g:NERDTreeCaseSensitiveSort = 1
    let g:NERDTreeQuitOnOpen = 1
    let g:NERDTreeWinPos = 'left'
    let g:NERDTreeShowBookmarks = 1
    let g:NERDTreeDirArrows=0
    let NERDTreeMinimalUI=1
    let NERDTreeIgnore = ['\.pyc$']
  "}}} Plugin: NERDTree

  "Plugin: NERDCommenter {{{
    " Add spaces after comment delimiters by default
    let g:NERDSpaceDelims = 1

    " Use compact syntax for prettified multi-line comments
    let g:NERDCompactSexyComs = 1

    " Align line-wise comment delimiters flush left instead of following code indentation
    let g:NERDDefaultAlign = 'left'

    " Set a language to use its alternate delimiters by default
    let g:NERDAltDelims_java = 1

    " Add your own custom formats or override the defaults
    let g:NERDCustomDelimiters = { 'c': { 'left': '/**','right': '*/' } }

    " Allow commenting and inverting empty lines (useful when commenting a region)
    let g:NERDCommentEmptyLines = 1

    " Enable trimming of trailing whitespace when uncommenting
    let g:NERDTrimTrailingWhitespace = 1
  "}}} Plugin: NERDCommenter

  "Plugin: ProjectRoot {{{
    function! <SID>AutoProjectRootCD()
      try
        if &ft != 'help'
          ProjectRootCD
        endif
      catch
        " Silently ignore invalid buffers
      endtry
    endfunction

    autocmd BufEnter * call <SID>AutoProjectRootCD()
  "}}} Plugin: ProjectRoot

  "Plugin: Tmuxline {{{
    " let g:tmuxline_separators = {
    "   \ 'left' : '',
    "   \ 'left_alt': '',
    "   \ 'right' : '',
    "   \ 'right_alt' : '',
    "   \ 'space' : ' '}
  "}}} Plugin: Tmuxline

  "Plugin: SexyScroller {{{
    let g:SexyScroller_MaxTime = 400
    let g:SexyScroller_EasingStyle = 3
  "}}} Plugin: SexyScroller

  "Plugin: UndoTree {{{
    nnoremap <F5> :UndotreeToggle<cr>
  "}}} Plugin: UndoTree

"}}} Plugins


