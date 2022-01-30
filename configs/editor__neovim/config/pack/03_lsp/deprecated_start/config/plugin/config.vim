set completeopt=menu,menuone,noselect

lua require("my-lsp")

lua <<EOF
local wk = require("which-key")
wk.register({
  ["<leader>l"] = { name = "+[L]SP" },
})
EOF

autocmd FileType TelescopePrompt lua require('cmp').setup.buffer { enabled = false }
