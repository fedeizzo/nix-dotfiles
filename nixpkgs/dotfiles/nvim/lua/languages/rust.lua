-- LSP
local lsp = require'lsp'
require'lspconfig'.rust_analyzer.setup {on_attach = lsp.on_attach}
