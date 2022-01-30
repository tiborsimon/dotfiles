"==============================================================================
" KEY MAPPINGS
"==============================================================================

" Using WhichKey only for documenting the pretty complex mappings. I didn't
" find a quick way to port it fully to WhichKey..

lua <<EOF
local wk = require("which-key")
wk.register({
  ["<leader>m"] = {
    name = "+Ledger [M]ode",
    h = { "Insert [H]eader" },
    t = { "Insert [T]ransaction" },
    p = { "Insert [P]osting" },
    c = { "Insert [C]losing Posting" },
    f = { "[F]ormat" },
  },
})
EOF

" HEADER - insert daily header with the current date.
nnoremap <buffer> <leader>mh Vc<cr><esc>i## <esc>77a=<esc>:r!date '+\%F - \%A'<cr>I## <esc>0o<esc>
" TRANSACTION - insert transaction header with the current date.
nnoremap <buffer> <leader>mt :read!date '+\%F'<cr>A * <esc>A
" POSTING - Add a transaction then prepare for amount entry.
nnoremap <buffer> <leader>mp :lua require("custom_pickers").ledger_posting()<cr>
" CLOSING POSTING - Add the final closing posting without amount entry.
nnoremap <buffer> <leader>mc :lua require("custom_pickers").ledger_closing_posting()<cr>

" Save the position to the 'l' mark, format the entire file then go back to
" the saved position.
nnoremap <buffer> <leader>mf :execute "normal! ml:%!my-ledger-formatter\r`l"<cr>


"==============================================================================
" OTHER CONFIG
"==============================================================================

" Custom ledger file format for speeddating.
1SpeedDatingFormat %Y-%m-%d - %A
