-- LSP
local lsp = require'lsp'
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)
require'lspconfig'.pyright.setup {
    on_attach = lsp.on_attach,
    capabilities = capabilities,
    python = {
        analysis = "strict",
    }
}

-- DAP
local dap = require('dap')

dap.adapters.python = {
  type = 'executable',
  command = 'python',
  args = {'-m', 'debugpy.adapter'},
}

dap.configurations.python = {
  {
    type = 'python',
    request = 'launch',
    name = 'Launch file',

    program = '${file}',
    pythonPath = 'python',
  },
}

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
    capabilities = capabilities
load_autocommands()
