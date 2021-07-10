vim.api.nvim_set_keymap('n', '<Leader>o', ':lua require\'telescope\'.git_files{}', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>l', ':lua require\'telescope\'.buffers{show_all_buffers = true}', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>s', ':lua require\'telescope\'.builtin{}', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>m', ':lua require\'telescope\'.tresitter{}', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>g', ':lua require\'telescope\'.git_bcommits{}', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>f', ':lua require\'telescope\'.live_grep{}', {noremap = true, silent = true})

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
