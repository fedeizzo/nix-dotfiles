vim.g.iron_map_defaults = 0
vim.g.iron_map_extended = 0

vim.api.nvim_set_keymap('n', '<Leader><CR>', ":lua require'iron'.core.send_line()<CR>", {noremap = true, silent = true})
vim.api.nvim_set_keymap('v', '<Leader><CR>', ":lua require'iron'.core.visual_send()<CR>", {noremap = true, silent = true})

local iron = require('iron')

iron.core.set_config {
    manager = 'tab_based',
    preferred = {
        python = 'ipython'
    }
}
