local pickers = require("telescope.pickers")
local finders = require("telescope.finders")
local conf = require("telescope.config").values
local actions = require("telescope.actions")
local action_state = require("telescope.actions.state")

local M = {}

M.ledger_posting = function(opts)
  pickers.new(opts, {
    prompt_title = "< Ledger Accounts >",
    finder = finders.new_oneshot_job({ "my-ledger-accounts" }, opts),
    sorter = conf.generic_sorter(opts),
    attach_mappings = function(prompt_bufnr, map)
      actions.select_default:replace(function()
        actions.close(prompt_bufnr)
        local selection = action_state.get_selected_entry()
        -- print(vim.inspect(selection))
        vim.api.nvim_put({ "    " .. selection[1] .. "   " }, "l", true, true)
        vim.cmd("normal A")  --                        ^
        -- Not completely perfect. Somehow the cursor will ends up here after the command
      end)
      return true
    end,
  }):find()
end

M.ledger_closing_posting = function(opts)
  pickers.new(opts, {
    prompt_title = "< Ledger Accounts >",
    finder = finders.new_oneshot_job({ "my-ledger-accounts" }, opts),
    sorter = conf.generic_sorter(opts),
    attach_mappings = function(prompt_bufnr, map)
      actions.select_default:replace(function()
        actions.close(prompt_bufnr)
        local selection = action_state.get_selected_entry()
        -- print(vim.inspect(selection))
        vim.api.nvim_put({ "    " .. selection[1], "" }, "l", true, true)
      end)
      return true
    end,
  }):find()
end

return M
