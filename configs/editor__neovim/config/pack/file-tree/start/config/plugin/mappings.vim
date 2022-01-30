" Global explorer toggle.
" nnoremap <silent> <leader>e :NvimTreeToggle<CR>

lua <<EOF
local wk = require("which-key")
wk.register({
  ["<leader>e"] = { "<cmd>NvimTreeToggle<cr>", "File [E]xplorer" },
})
EOF
