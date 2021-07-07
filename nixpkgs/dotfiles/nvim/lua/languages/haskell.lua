-- LSP
local lsp = require'lsp'
require'lspconfig'.hls.setup {on_attach = lsp.on_attach}

-- AUTOCOMMANDS 
local function load_autocommands()
  local autogroups = {
    filetypes = {
      {
        'FileType',
        'haskell',
        'setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2',
      },
    },
  }
  createAutogroups(autogroups)
end
load_autocommands()
