-- LSP
local lsp = require'lsp'
require'lspconfig'.pyright.setup {on_attach = lsp.on_attach}

-- AUTOCOMMANDS 
local function load_autocommands()
  local autogroups = {
    buffers = {
      {'BufNewFile', '*.py', 'set autoindent'},
      {'BufRead', '*.py', 'set autoindent'},
    },
  }
  createAutogroups(autogroups)
end
load_autocommands()