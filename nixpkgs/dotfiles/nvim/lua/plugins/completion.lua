vim.g.completopt = {'menuone', 'noinsert', 'noselect'}
vim.opt.shortmess = 'c'

vim.api.nvim_set_keymap('i', '<expr> <Tab>', 'pumvisible() ? "<C-n>" : "<Tab>"', {noremap = true})
vim.api.nvim_set_keymap('i', '<expr> <S-Tab>', 'pumvisible() ? "<C-p>" : "<S-Tab>"', {noremap = true})

vim.g.completion_enable_snippet = 'vim-vsnip'
vim.g.completion_auto_change_source = 1

vim.g.completion_chain_complete_list = {
  default = {
    default = {
      {complete_items = {'lsp', 'snippet', 'path'}},
      {mode = '<c-p>'},
      {mode = '<c-n>'},
    },
    comment = {},
    string = {},
  },
  typescript = {
    {complete_items = {'lsp', 'snippet', 'ts', 'path'}},
    {mode = '<c-p>'},
    {mode = '<c-n>'},
  },
  bash = {
    {complete_items = {'lsp', 'snippet', 'ts', 'path'}},
    {mode = '<c-p>'},
    {mode = '<c-n>'},
  },
  python = {
    {complete_items = {'lsp', 'snippet', 'ts', 'path'}},
    {mode = '<c-p>'},
    {mode = '<c-n>'},
  },
  lua = {
    {complete_items = {'snippet', 'ts', 'path'}},
    {mode = '<c-p>'},
    {mode = '<c-n>'},
  },
}
