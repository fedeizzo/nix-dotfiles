-- LSP
local lsp = require'lsp'
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)
require'lspconfig'.tsserver.setup {
    on_attach = lsp.on_attach,
    capabilities = capabilities
}

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
