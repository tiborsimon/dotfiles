"==============================================================================
" SEARCH CONFIG
"==============================================================================

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
" nnoremap <silent> <leader>/ :nohlsearch<cr>
