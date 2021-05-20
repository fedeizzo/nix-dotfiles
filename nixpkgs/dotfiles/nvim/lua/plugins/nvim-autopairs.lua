local remap = vim.api.nvim_set_keymap
local npairs = require('nvim-autopairs')

-- <CR> REMAP
_G.MUtils = {}

vim.g.completion_confirm_key = ''

MUtils.completion_confirm = function()
  if vim.fn.pumvisible() ~= 0 then
    if vim.fn.complete_info()['selected'] ~= -1 then
      require'completion'.confirmCompletion()
      return npairs.esc('<c-y>')
    else
      vim.api.nvim_select_popupmenu_item(0, false, false, {})
      require'completion'.confirmCompletion()
      return npairs.esc('<c-n><c-y>')
    end
  else
    return npairs.autopairs_cr()
  end
end

remap(
  'i', '<CR>', 'v:lua.MUtils.completion_confirm()',
  {expr = true, noremap = true}
)

-- TREESITTER SUPPORT
local npairs = require('nvim-autopairs')

npairs.setup({check_ts = true})

require('nvim-treesitter.configs').setup {autopairs = {enable = true}}

local ts_conds = require('nvim-autopairs.ts-conds')
require('nvim-autopairs').enable()
