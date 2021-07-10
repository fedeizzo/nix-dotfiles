vim.api.nvim_set_keymap('i', '<expr> <C-j>', 'vsnip#available(1) ? \'<Plug>(vsnip-expand)\' : \'<C-j>\'', {})
vim.api.nvim_set_keymap('i', '<expr> <C-l>', 'vsnip#available(1) ? \'<Plug>(vsnip-expand-or-jump)\' : \'<C-l>\'', {})
vim.api.nvim_set_keymap('i', '<expr> <C-j>', 'vsnip#available(1) ? \'<Plug>(vsnip-jump-next)\' : \'<Tab>\'', {})
vim.api.nvim_set_keymap('i', '<expr> <C-k>', 'vsnip#available(-1) ? \'<Plug>(vsnip-jump-prev)\' : \'<S-Tab>\'', {})
vim.api.nvim_set_keymap('s', '<expr> <C-j>', 'vsnip#available(1) ? \'<Plug>(vsnip-jump-next)\' : \'<Tab>\'', {})
vim.api.nvim_set_keymap('s', '<expr> <C-l>', 'vsnip#available(1) ? \'<Plug>(vsnip-expand-or-jump)\' : \'<C-l>\'', {})
vim.api.nvim_set_keymap('s', '<expr> <C-k>', 'vsnip#available(-1) ? \'<Plug>(vsnip-jump-prev)\' : \'<S-Tab>\'', {})

vim.g.vsnip_snippet_dir = vim.fn.expand('~/.config/nvim/snippet')
