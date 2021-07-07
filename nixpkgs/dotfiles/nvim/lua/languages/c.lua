-- LSP
local lsp = require'lsp'
require'lspconfig'.ccls.setup {on_attach = lsp.on_attach}
