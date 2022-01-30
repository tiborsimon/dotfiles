"==============================================================================
" KEY MAPPINGS
"==============================================================================

" Using WhichKey only for documenting the pretty complex mappings. I didn't
" find a quick way to port it fully to WhichKey..

lua <<EOF
local wk = require("which-key")
wk.register({
  ["<leader>m"] = {
    name = "+Python [M]ode",
    -- e = { "[E]xecute" },
    d = { "Insert [D]ebug" },
  },
})
EOF

" Since vim 8 introduced the terminal mode, executing an external command
" won't run in a full size window, instead it will appear in the command line,
" making it hardly usable in as a python code executor. I figured that having
" a separate terminal process would be better for my purposes, so I wint with
" 'toggleterm'.
" nnoremap <buffer> <leader>me :! clear && python3 %<cr>
nnoremap <buffer> <leader>md Oimport pdb; pdb.set_trace()<esc>j^:w<cr>
