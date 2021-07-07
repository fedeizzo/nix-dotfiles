-- LSP
local lsp = require'lsp'
require'lspconfig'.bashls.setup {on_attach = lsp.on_attach}
