-- LSP
require'lsp'
require'lspconfig'.tsserver.setup {on_attach = on_attach}

-- AUTOCOMMANDS 
local function load_autocommands()
  local autogroups = {
    filetypes = {
      {
        'FileType',
        'typescript',
        'setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2',
      },
    },
  }
  createAutogroups(autogroups)
end
load_autocommands()
