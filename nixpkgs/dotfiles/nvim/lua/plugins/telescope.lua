vim.api.nvim_set_keymap('n', '<Leader>o', ':lua require\'telescope.builtin\'.git_files{}<CR>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>l', ':lua require\'telescope.builtin\'.buffers{show_all_buffers = true}<CR>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>s', ':lua require\'telescope.builtin\'.builtin{}<CR>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>m', ':lua require\'telescope.builtin\'.treesitter{}<CR>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>g', ':lua require\'telescope.builtin\'.git_bcommits{}<CR>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>f', ':lua require\'telescope.builtin\'.live_grep{}<CR>', {noremap = true, silent = true})

require('telescope').setup {
  defaults = {
    layout_strategy = 'flex',
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
    },
  },
}
