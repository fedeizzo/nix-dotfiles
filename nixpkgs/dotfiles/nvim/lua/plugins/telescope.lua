vim.api.nvim_set_keymap('n', '<Leader>o', ':lua require\'telescope.builtin\'.git_files{}<CR>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>l', ':lua require\'telescope.builtin\'.buffers{show_all_buffers = true}<CR>', {noremap = true, silent = true})
-- vim.api.nvim_set_keymap('n', '<Leader>s', ':lua require\'telescope.builtin\'.builtin{}<CR>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>m', ':lua require\'telescope.builtin\'.treesitter{}<CR>', {noremap = true, silent = true})
-- vim.api.nvim_set_keymap('n', '<Leader>g', ':lua require\'telescope.builtin\'.git_bcommits{}<CR>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>f', ':lua require\'telescope.builtin\'.live_grep{}<CR>', {noremap = true, silent = true})

require('telescope').setup {
  defaults = {
    vimgrep_arguments = {
       "rg",
       "--color=never",
       "--no-heading",
       "--with-filename",
       "--line-number",
       "--column",
       "--smart-case",
    },
    prompt_prefix = "   ",
    selection_caret = "  ",
    entry_prefix = "  ",
    selection_strategy = "reset",
    sorting_strategy = "descending",
    initial_mode = "insert",
    layout_strategy = 'flex',
    file_sorter = require("telescope.sorters").get_fuzzy_file,
    file_ignore_patterns = {
      '%.png$',
      '%.tif$',
      '%.tiff$',
      '%.shp$',
      '%.dbf$',
      '%.qix$',
      '%.shx$',
      '%.qpj$',
      '%.prj$',
      '%.cpg$',
      '.ccls-cache',
      'node_modules'
    },
    generic_sorter = require("telescope.sorters").get_generic_fuzzy_sorter,
    path_display = { "truncate" },
    winblend = 0,
    border = {},
    borderchars = { "─", "│", "─", "│", "╭", "╮", "╯", "╰" },
    color_devicons = true,
    use_less = true,
    set_env = { ["COLORTERM"] = "truecolor" },
    file_previewer = require("telescope.previewers").vim_buffer_cat.new,
    grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
    qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,
  },
}
