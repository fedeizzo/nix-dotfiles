-- LSP
local lsp = require'lsp'
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)
require'lspconfig'.clojure_lsp.setup {
    on_attach = lsp.on_attach,
    capabilities = capabilities,
    filetypes = { "clojure", "edn", "fennel" }
}
