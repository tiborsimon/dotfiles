lua <<EOF
local wk = require("which-key")
wk.register({
  ["<leader>g"] = { name = "+[G]it" },
  ["<leader>gc"] = { "<cmd>Telescope git_bcommits<cr>", "Git [C]ommits for buffer" },
  ["<leader>gC"] = { "<cmd>Telescope git_commits<cr>", "Git [C]ommits" },
  ["<leader>gs"] = { "<cmd>Telescope git_status<cr>", "Git [S]tatus" },
  ["<leader>gb"] = { "<cmd>Telescope git_branch<cr>", "Git [B]ranch" },
})
EOF
