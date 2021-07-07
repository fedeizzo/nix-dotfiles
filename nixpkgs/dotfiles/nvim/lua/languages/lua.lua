-- LSP
local lsp = require'lsp'

-- AUTOCOMMANDS 
local function load_autocommands()
  local autogroups = {
    filetypes = {
      {
        'FileType',
        'lua',
        'setlocal expandtab tabstop=2 shiftwidth=2 softtabstop=2',
      },
    },
  }
  createAutogroups(autogroups)
end
load_autocommands()
