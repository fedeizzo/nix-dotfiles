-- LSP
local lsp = require'lsp'
require'lspconfig'.dockerls.setup {on_attach = lsp.on_attach}
