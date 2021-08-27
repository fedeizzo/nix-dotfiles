require('utils/nvim-core')

vim.g.mapleader = ' '
-- ENCODING
vim.opt.encoding = 'utf-8'
vim.opt.fileencoding = 'utf-8'
-- WILD MENU
vim.opt.wildignore = vim.opt.wildignore + {'*.a', '*.o', '*.bmp', '*.gif', '*.ico', '*.jpg', '*.png', '.DS_Store', '.git', '.hg', '.svn', '*~', '*.swp', '*.tmp'}
vim.opt.wildmenu = true
-- INDENT
vim.opt.autoindent = true
vim.opt.smartindent = true
-- TAB
vim.opt.tabstop = 4
vim.opt.shiftwidth = 4
vim.opt.expandtab = true
vim.opt.showtabline = 2
vim.opt.foldmethod = 'expr'
vim.opt.foldexpr = 'nvim_treesitter#foldexpr()'
-- FOLD
vim.opt.foldenable = false
-- LINE NUMBER
vim.opt.number = true
vim.opt.relativenumber = true
-- MOUSE
vim.opt.mouse = 'a'
-- SPLIT
vim.opt.splitbelow = true
-- SEARCH HIGHLIGHT
vim.opt.hlsearch = true
-- SPELL CHECK
vim.opt.spell = false
-- vim.opt.spelllang = vim.opt.spelllang + {'it'}
-- COLORSCHEME
vim.cmd [[colorscheme nord]]
vim.opt.termguicolors = true

vim.cmd [[filetype plugin indent on]]
vim.bo.omnifunc = "v:lua.vim.lsp.omnifunc"

local function load_autocommands()
  local autogroups = {
    filetypes = {
      {'FileType', 'git', 'setlocal nospell'},
      {'FileType', 'qf', 'setlocal nospell'},
    },
  }
  createAutogroups(autogroups)
end

vim.api.nvim_set_keymap('n', '<Leader>h', ':noh<CR>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>dd', ':bd<CR>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>y', '"+', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>c', ':lua automaticOutput()<CR>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>e', ':lua enumeratePython()<CR>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>a', ':w<CR>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<Leader>q', ':q<CR>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', '<C-h>', '<C-w><C-h>', {noremap = true})
vim.api.nvim_set_keymap('n', '<C-j>', '<C-w><C-j>', {noremap = true})
vim.api.nvim_set_keymap('n', '<C-k>', '<C-w><C-k>', {noremap = true})
vim.api.nvim_set_keymap('n', '<C-l>', '<C-w><C-l>', {noremap = true})
vim.api.nvim_set_keymap('v', '<Leader>y', '"+y<CR>', {noremap = true, silent = true})

load_autocommands()
