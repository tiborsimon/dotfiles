lua <<EOF
local wk = require("which-key")
wk.register({
  ["<leader>ff"] = { "<cmd>Telescope find_files<cr>", "[F]ind Files" },
  ["<leader>fr"] = { "<cmd>Telescope oldfiles<cr>", "Open [R]ecent Files" },
  ["<leader>fn"] = { "<cmd>enew<cr>", "[N]ew File" },
})
wk.register({
  ["<leader>/"] = { name = "+Search [/]" },
  ["<leader>//"] = { "<cmd>nohlsearch<cr>", "Remove search highlighting" },
  ["<leader>/l"] = { "<cmd>Telescope live_grep<cr>", "[L]ive Grep" },
  ["<leader>/s"] = { "<cmd>Telescope grep_string<cr>", "Grep [S]tring Under Cursor" },
})
wk.register({
  ["<leader>b"] = { "<cmd>Telescope buffers<cr>", "[B]uffers" },
  ["<leader>//"] = { "<cmd>nohlsearch<cr>", "Remove search highlighting" },
  ["<leader>/l"] = { "<cmd>Telescope live_grep<cr>", "[L]ive Grep" },
  ["<leader>/s"] = { "<cmd>Telescope grep_string<cr>", "Grep [S]tring Under Cursor" },
})
EOF
