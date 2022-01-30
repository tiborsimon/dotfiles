lua <<EOF
local actions = require('telescope.actions')
-- Global remapping
------------------------------
require('telescope').setup{
  pickers = {
    find_files = {
      layout_config = {
        scroll_speed = 1,
        width = 0.8,
      },
    },
    git_commits = {
      layout_config = {
        scroll_speed = 1,
      },
    },
    git_bcommits = {
      layout_config = {
        scroll_speed = 1,
      },
    },
    git_status = {
      layout_config = {
        scroll_speed = 1,
      },
    },
    git_branch = {
      layout_config = {
        scroll_speed = 1,
      },
    },
  },
  defaults = {
    mappings = {
      i = {
        ["<C-e>"] = actions.preview_scrolling_down,
        ["<C-y>"] = actions.preview_scrolling_up,
        ["<C-j>"] = actions.move_selection_next,
        ["<C-k>"] = actions.move_selection_previous,
        ["<C-n>"] = actions.cycle_previewers_next,
        ["<C-p>"] = actions.cycle_previewers_prev,
        ["<C-v>"] = actions.select_horizontal,
        ["<C-s>"] = actions.select_vertical,
        ["<C-t>"] = actions.select_tab,
      },
      n = {
        ["q"] = actions.close,
      },
    },
  }
}

require('telescope').load_extension('fzf')
EOF
